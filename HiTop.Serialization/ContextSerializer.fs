module internal HiTop.Serialization.ContextSerializer

module private Nodes =
    let magicString = {
        write = fun _ writer ->
            writer.Write(MagicSignature)

        read = fun context reader ->
            let magic = reader.ReadBytes(MagicSignature.Length)
            if magic <> MagicSignature then
                Failure(BadMagicSignature)
            else
                Success(context)
    }

    let fileSize = {
        write = fun context writer ->
            writer.Write(context.FileSize)

        read = fun context reader ->
            let size = reader.ReadInt32()
            if size < 0 then
                Failure(BadFileSize)
            else
                { context with FileSize = size }
                |> Success
    }

    let fileName = {
        write = fun context writer ->
            writer.Write(context.FileName)

        read = fun context reader ->
            let name = reader.ReadString()
            if name.Length <= 0 then
                Failure(BadFileName)
            else
                { context with FileName = name }
                |> Success
    }

    let program = {
        write = fun context writer ->
            writer.Write(context.Program)

        read = fun context reader ->
            let pos = reader.BaseStream.Position |> int
            let length = reader.BaseStream.Length |> int
            let rest = length - pos

            let program = reader.ReadBytes(rest)

            { context with Program = program }
            |> Success
    }

    let all =
        magicString :: fileSize :: fileName :: program :: []


let write writer context =
    Nodes.all
    |> List.iter (fun x -> x.write context writer)

let read reader =
    let rec apply ctx nodes =
        match nodes with
        | [] -> Success ctx
        | head::rest ->
            let result = head.read ctx reader
            match result with
            | Failure x -> Failure x
            | Success ctx' -> apply ctx' rest
        
    apply Context.empty Nodes.all