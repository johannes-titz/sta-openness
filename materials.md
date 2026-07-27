# Study materials

This file documents the text-based materials and experimental procedure for
*Perceived Liberalism and Intellect Cannot Be Reduced to a Single Openness
Dimension: A State-Trace Experiment*. Together with the article's Methods and
Appendix, it provides the information needed to reproduce the vignette
procedure. The study used no images, audio, video, physical apparatus, or
proprietary stimulus software.

## Design and presentation

The experiment used a 2 x 3 fully within-subject design:

- self-efficacy: low or high;
- artistic interest: low, medium, or high.

Every participant judged all six conditions. Condition order was randomized
separately for each participant. The first randomized condition was also used
as a between-subjects comparison because it could not be affected by exposure
to earlier vignettes.

The fictitious target was called Ms. N. Participants were told that Ms. N. had
completed a personality questionnaire, that her answers were accurate
self-descriptions, and that the response scale was:

| Value | English label | German label |
|---:|---|---|
| 1 | inaccurate | trifft nicht zu |
| 2 | moderately inaccurate | trifft eher nicht zu |
| 3 | neither inaccurate nor accurate | weder noch |
| 4 | moderately accurate | trifft eher zu |
| 5 | accurate | trifft zu |

The English rendering of the vignette introduction used in the article was:

> Ms. N. completes a questionnaire designed to assess her personality. She
> indicates the extent to which various statements apply to her, using a scale
> ranging from one to five. You will now see a selection of statements from the
> questionnaire. Assume that Ms. N. is entirely "correct" in her
> self-assessments: that is, the statements apply to her exactly to the extent
> indicated by her ratings.

## Manipulated vignette items

The same rating was applied to all three items within a manipulated facet. The
German wording below is the wording reported for the administered study
materials. "Ich" was added where needed to make the self-assessment context
explicit.

### Self-efficacy

| Item | English rendering | German wording |
|---|---|---|
| C35 | I excel in what I do. | Ich bin hervorragend in dem, was ich tue. |
| C65 | I handle tasks smoothly. | Ich erledige Aufgaben elegant/reibungslos. |
| C95 | I know how to get things done. | Ich verstehe es, Dinge zu erledigen. |

- Low self-efficacy: all three items rated 1 ("inaccurate").
- High self-efficacy: all three items rated 5 ("accurate").

### Artistic interest

| Item | English rendering | German wording |
|---|---|---|
| O08 | I believe in the importance of art. | Ich glaube an die Wichtigkeit von Kunst. |
| O38 | I see beauty in things that others might not notice. | Ich sehe Schönheit in Dingen, die anderen möglicherweise nicht auffällt. |
| O68 | I like poetry. | Ich mag Gedichte. |

Item O68 was reversed from the negatively keyed original so that all three
stimulus items were positively keyed.

- Low artistic interest: all three items rated 1 ("inaccurate").
- Medium artistic interest: all three items rated 3 ("neither inaccurate nor accurate").
- High artistic interest: all three items rated 5 ("accurate").

The six conditions therefore were:

| Condition | Data key | Self-efficacy | Artistic interest |
|---:|---|---|---|
| 1 | C10O1 | low | high |
| 2 | C11O0 | high | low |
| 3 | C10OM | low | medium |
| 4 | C11OM | high | medium |
| 5 | C11O1 | high | high |
| 6 | C10O0 | low | low |

## Outcome measures

After each vignette, participants rated Ms. N. on four liberalism items and
four intellect items using the same five-point response scale. Items were
phrased in the third person for the target-rating task.

### Liberalism

| Item | English rendering | German wording |
|---|---|---|
| O28 | Tends to vote for liberal political candidates. | Neigt dazu, linksliberale Politiker zu wählen. |
| O58 | Believes that there is no absolute right or wrong. | Glaubt, dass es kein absolutes richtig oder falsch gibt. |
| O88 | Tends to vote for conservative political candidates. | Tendiert dazu, konservative Politiker zu wählen. |
| O118 | Believes that we should be tough on crime. | Glaubt, dass wir Verbrechen hart ahnden sollten. |

### Intellect

| Item | English rendering | German wording |
|---|---|---|
| O23 | Loves to read challenging or complex texts. | Liest gerne anspruchsvolle/komplexe Texte. |
| O53 | Avoids philosophical discussions. | Vermeidet philosophische Diskussionen. |
| O83 | Has difficulty understanding abstract ideas. | Hat Schwierigkeiten, abstrakte Ideen zu verstehen. |
| O113 | Is not interested in theoretical discussions. | Ist wenig an theoretischen Diskussionen interessiert. |

Negatively keyed items were reverse-scored before summing. Each four-item
facet score therefore ranged from 4 to 20.

## Manipulation and attention checks

Participants also rated the target on two manipulation-check items that were
not included in the vignette:

| Item | Construct | English rendering | German wording |
|---|---|---|---|
| C05 | Self-efficacy | Completes tasks successfully. | Schließt Aufgaben erfolgreich ab. |
| O98 | Artistic interest | Enjoys going to art museums. | Geht gerne in Kunstmuseen. |

Immediately after the first vignette, participants reproduced Ms. N.'s rating
on C35. Additional attention checks were embedded among the liberalism and
intellect ratings and instructed participants to select a specified response
(for example, "This question checks your attention. Please select 5 =
accurate.").

## Data and code correspondence

- `between.csv` contains the first randomized vignette condition used for the
  between-subjects analysis.
- `within.csv` contains all six vignette conditions used for the
  within-subjects analysis.
- `Main_Study/Data/Data_Main.csv` contains the exported main-study data.
- `Main_Study/Data/Data_Documentation` maps conditions, items, columns, and
  attention-check fields.
- `README.md` reproduces the article's primary analyses.
- `Main_Study/Scripts/R_Script_Main.R` contains the more detailed original
  main-study analysis workflow.

The German item translations were adapted from Dege (2018) and Treiber et al.
(2013), as documented in the article Appendix. The underlying IPIP-NEO-120
item framework is described by Johnson (2014).
