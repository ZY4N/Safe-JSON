import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Files;
import org.json.JSONObject;

public class OrgJson {
	public static void main(String args[]) throws IOException {
		final var text = Files.readString(Path.of(args[0]));

		final var begin_parse = System.nanoTime();
		final var obj = new JSONObject(text);
		final var end_parse = System.nanoTime();

		final var begin_serialize = System.nanoTime();
		final var str = obj.toString();
		final var end_serialize = System.nanoTime();
		
		System.out.println((end_parse - begin_parse) / 1000.0);
		System.out.println((end_serialize - begin_serialize) / 1000.0);
	}
}
