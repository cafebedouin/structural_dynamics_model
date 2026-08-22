% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__modernization_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orthographic_kernel__modernization_reading
 *   human_readable: Turkish Latin-Script Orthography Mandate (Modernization Reading)
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   From the modernization reading's own lights, the standing arrangement
 *   under assessment is the mandatory Latin-script orthography instituted by
 *   the 1928 alphabet law together with the literacy machinery built to run
 *   it. The epsilon referent is that standing arrangement as this reading
 *   assesses it — not the Arabic-script arrangement the continuity reading
 *   would restore, and not the counterfactual in which the modernizers'
 *   motives were concealment. The reading counts real transition costs borne
 *   by named groups and judges the net arrangement a modernization
 *   instrument; the metrics below are authored independently of that claim,
 *   and the claimed type (tangled_rope) states what I take to be structurally
 *   true of the arrangement: a genuine encoding-and-literacy coordination
 *   function carrying asymmetric, actively enforced costs. KEY AGENTS (by
 *   structural relationship): - state_bureaucracy: agenda-setter and
 *   principal collector (institutional/arbitrage) — designed the law, runs
 *   the schools and registries that operate in the new script -
 *   new_literate_class: primary beneficiary (organized/mobile) — credential,
 *   professions, direct access to Latin-script science -
 *   rural_literacy_recruits: mass beneficiary bearing campaign costs
 *   (powerless/constrained) — compulsory course attendance traded for first
 *   literacy - ottoman_script_literates: primary payer among the old elite
 *   (moderate/trapped) — script-specific human capital stranded in 1928 -
 *   arabic_script_printers_calligraphers: occupational payers
 *   (moderate/constrained) — trade collapsed with the deadlines -
 *   religious_textual_intermediaries: identity-locked payers
 *   (moderate/identity_locked) — standing constituted by the Arabic-script
 *   textual formation - continuity_minded_educators: excluded voice
 *   (moderate/constrained) — proposed gradual or vowel-marked compromises,
 *   not consulted - linguistic_historians: analytical observer
 *   (analytical/analytical) — tracks the century-long record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, 0.45).
domain_priors:suppression_score(orthographic_kernel__modernization_reading, 0.35).
domain_priors:theater_ratio(orthographic_kernel__modernization_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "Turkish Latin-Script Orthography Mandate (Modernization Reading)").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, '511df798-5b3a-44fb-ab22-8c91a22b102c').
narrative_ontology:cs_kernel_codification('511df798-5b3a-44fb-ab22-8c91a22b102c', formalized).
narrative_ontology:cs_authority_grounding('511df798-5b3a-44fb-ab22-8c91a22b102c', extraction).
narrative_ontology:cs_interpretation_layer_present('511df798-5b3a-44fb-ab22-8c91a22b102c').
narrative_ontology:cs_reading_relation('511df798-5b3a-44fb-ab22-8c91a22b102c', orthographic_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('511df798-5b3a-44fb-ab22-8c91a22b102c', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('511df798-5b3a-44fb-ab22-8c91a22b102c', foundational, script_legitimacy_from_modernization_efficacy).
narrative_ontology:cs_axiom_status(script_legitimacy_from_modernization_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('511df798-5b3a-44fb-ab22-8c91a22b102c', script_legitimacy_from_modernization_efficacy, instrumental).
narrative_ontology:cs_axiom('511df798-5b3a-44fb-ab22-8c91a22b102c', foundational, phonemic_orthography_literacy_supremacy).
narrative_ontology:cs_axiom_status(phonemic_orthography_literacy_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('511df798-5b3a-44fb-ab22-8c91a22b102c', phonemic_orthography_literacy_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('511df798-5b3a-44fb-ab22-8c91a22b102c', secondary, identity_carried_by_phonemic_adaptation).
narrative_ontology:cs_axiom_status(identity_carried_by_phonemic_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('511df798-5b3a-44fb-ab22-8c91a22b102c', identity_carried_by_phonemic_adaptation, conventional).
narrative_ontology:cs_reference_frame('511df798-5b3a-44fb-ab22-8c91a22b102c', latin_modernization_settlement).
narrative_ontology:cs_drift_state('511df798-5b3a-44fb-ab22-8c91a22b102c', contemporary_ottoman_revival, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('511df798-5b3a-44fb-ab22-8c91a22b102c', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_literate_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, rural_literacy_recruits).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, ottoman_script_literates).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, arabic_script_printers_calligraphers).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, religious_textual_intermediaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, rural_literacy_recruits).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, phonemic_literacy_acceleration_thesis).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, latin_script_technical_alignment_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and passed the 1928 alphabet laws, set the deadlines for public signage and official documents, and ran the national literacy courses (Millet Mektepleri) with attendance obligations for adults who could not read the new script. After the switch, every census roll, tax record, court file, and schoolbook existed only in the new script, giving the ministries a uniformly legible population to administer. The ministries that designed the change also staff the schools, courts, and registries that run on it; revisiting the choice would mean retraining their own workforce and reorganizing a century of records.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% The cohort schooled entirely in the new alphabet — teachers, clerks, officers, engineers, journalists. The script they learned in months opened state employment, the professions, and direct reading of Latin-alphabet scientific and technical literature. Their skills are portable within the national system and they staff the institutions that run on the alphabet; their parents' Ottoman-script schooling became a private accomplishment rather than a credential.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_literate_class, beneficiary,
    organized, biographical, mobile, national).

% Adults literate in Ottoman Turkish when the law passed. Within months their reading and writing no longer served for official business, newspapers, or their children's schoolbooks; staying literate meant retraining in evening courses while working, and many never did. Their access to their own correspondence, libraries, and the Ottoman archive now runs through specialists. Age and sunk schooling made retraining costlier the older they were.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ottoman_script_literates, payer,
    moderate, biographical, trapped, national).

% Printers holding Arabic-type cases, calligraphers, manuscript copyists, and the scribes who drafted petitions for an illiterate clientele. The market for the trade collapsed with the signage and publishing deadlines; some presses retooled to Latin type, others closed. The craft skills transferred only partially — composition in the new type, or decorative and touristic calligraphy.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, arabic_script_printers_calligraphers, payer,
    moderate, biographical, constrained, national).

% Prayer leaders, Qur'an-course teachers, Sufi sheikhs, and scholars whose standing rested on command of Arabic-script texts — the Qur'an, commentaries, Ottoman poetry and chronicles. The reform did not touch the Arabic Qur'an itself, but it cut the vernacular textual bridge (Ottoman Turkish in Arabic letters) through which they had reached lay readers, and the new state schools competed for the same hours. Their role was constituted by the old textual formation; leaving it would have meant ceasing to be what they were, so most continued in a shrinking orbit of mosque and course.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, religious_textual_intermediaries, payer,
    moderate, generational, identity_locked, national).

% Villagers, mostly adults swept into the 1930s campaigns, required to attend literacy courses and in some districts fined or pressured for absence. They bore the attendance burden and the taxes that funded schools, and in return many gained first-time literacy in a script that maps Turkish sounds one-to-one — easier to acquire than the old one, and the passport to army forms, land registries, and market paperwork. Whether the bargain paid depended heavily on district school provision.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, rural_literacy_recruits, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, rural_literacy_recruits, payer).

% Teachers, historians, and men of letters who argued in 1928 for a gradual bilingual transition or for a vowel-marked Arabic alphabet, and who kept teaching the old script privately after it left the schools. They were not consulted on the law's design; their proposals survive in contemporary journals and memoirs rather than in the assembly record that mattered. Some later staffed the Ottoman-script electives reintroduced in the 2010s.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, continuity_minded_educators, excluded,
    moderate, generational, constrained, national).

% Scholars inside and outside Turkey who track literacy statistics, school enrollment, publication output, and archival access across the reform, and who compare Turkey's trajectory with the Persian, Urdu, and Azerbaijani script histories. They can read the pre-1928 record directly and assess what the switch cost and delivered; they administer nothing and collect nothing.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_kernel__modernization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, phonemically exact writing system for Turkish that can be taught in months rather than years, set with standard Latin type, and read alongside the Latin-alphabet scientific and technical literature; it gives the state, schools, press, and markets one shared encoding of the language.
% TRANSFER_FUNCTION: Moves textual authority and administrative access from the Ottoman-script literate elite to the state-certified new literate class; moves the one-time costs of the switch — retraining, displaced trades, severed access to Ottoman texts — onto the transition generation; and moves mass literacy from the state's aspiration to the population.
% ABSENT_VOICES: The continuity-minded educators, Ottoman-script authors and publishers, and religious textual communities who favored retention or a vowel-marked Arabic compromise were outside the 1928 decision; the adults conscripted into literacy courses had no seat in the law's drafting. Their positions survive in contemporary journals, memoirs, and exile publications.
% DISAPPEARANCE_RATIONALE: If the Latin orthography mandate disappeared overnight, official records, schooling, publishing, signage, and the digital infrastructure built on a century of Latin-script Turkish would all need re-provisioning; the state's administrative legibility, the school system's shared encoding, and the new literate class's credential would lose their substrate. The arrangement is load-bearing for the modern sector this reading celebrates.
% FOUNDING_PROBLEM: Ottoman Turkish written in the Arabic script marked few vowels, mapped several letters to single sounds, and took years to master; the new republic wanted a population that could be taught to read quickly, an administration that could read its population, and direct access to Latin-alphabet science.
% FOUNDING_PROBLEM_CORROBORATION: Cross-linguistic reading research outside Turkey corroborates that phonemic orthographies are acquired faster than deep ones, and 1930s foreign educational observers attested the campaigns' reach. But the claim that the problem required full replacement — rather than vowel-marking the Arabic script, as some contemporaries proposed — is only partially attested and disputed; no source outside the benefiting parties establishes the necessity of this particular remedy, and the sibling readings dispute that literacy was the operative problem at all. Stated plainly: partial external corroboration for the problem's reality, none for the necessity of the chosen solution.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__modernization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__modernization_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__modernization_reading_tests).
:- end_tests(orthographic_kernel__modernization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 — moderate: this reading counts the transition generation's stranded literacy, the collapsed script trades, and the displaced textual intermediaries as real costs borne by named groups, while the phonemic script's literacy and alignment gains are a genuine coordination yield the reading does not discount. Suppression (0.35) is authored as the current structural state: the Arabic script remains legally excluded from official documents, schooling, and mainstream publishing, but the machinery that once policed the switch is largely dormant — hence its divergence from the suppression_requirement series, which traces active enforcement capacity and decays from its 1935 peak (0.72) to 0.18 as compliance became self-sustaining. Theater_ratio is low (0.26): the literacy courses taught, the schools still teach, and only a commemorative layer (alphabet anniversaries, language festivals) is performative; the slow rise from 0.10 tracks the campaign's completion. Accessibility_collapse is 0.55: official alternatives collapsed completely, but private, religious, and heritage use of the old script persisted and now returns as elective teaching. Resistance is 0.40: real founding-era opposition (deputies, religious conservatives, Ottoman literati) faded into periodic cultural-political flare-ups rather than sustained resistance. All three series share one six-point grid (1928-2026) so the engine samples every metric at every authored point. The enforcement trajectory, not a flat picture, is this story's traced dynamic, which is why suppression_requirement is authored at all.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the state_bureaucracy seat the arrangement is its own working machinery — the ministries that set the deadlines are the ministries that read the records — so effective extraction sits near the beneficiary end. From the ottoman_script_literates seat the same arrangement is a lifetime's human capital devalued overnight with no exit at any price; from the religious_textual_intermediaries seat it is the dissolution of a role those agents cannot leave without ceasing to be what they are — an identity lock that would loosen only if the old script became a paid heritage specialty rather than a constitutive identity. The same-level lateral contrast is between two national literate classes of equal nominal standing: the new literate class (mobile; its skills are the system's currency) and the Ottoman-script literates (trapped; their skills stranded) — the differentiating factor is script-specific capital and timing, not power. The engine computes these per-seat classifications from the structural data; this reading's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: state_bureaucracy (collects administrative legibility and a standardized credential market), new_literate_class (collects credentials, professions, and direct scientific access), rural_literacy_recruits (collect first literacy, pay attendance and taxes — a dual-positioned seat the derivation should place near-symmetric, slightly beneficiary-side). Victims declared: ottoman_script_literates, arabic_script_printers_calligraphers, religious_textual_intermediaries — all with constrained-to-locked exits, placing them near the full-target end. Receipt: the arrangement's gains demonstrably accrue to the state_bureaucracy seat — it set the terms and collects the capacity — so gain_flow names it rather than 'diffuse'; the new literate class benefits without capturing. No directionality overrides are authored: the beneficiary/victim plus exit data already differentiate the seats, and overrides key on power atoms that are shared across differently-positioned seats here (moderate power spans trapped payers and the excluded educators), so an override would misfire across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding campaign — teaching a nation to read in its new script — was substantially complete by mid-century, and the arrangement outlived it. But this is not a mandate outliving its function: the orthography still performs daily as the national encoding, so mandatrophy is not resolved and the theater_ratio stays low. The classification discipline cuts both ways. Declaring the victims and the enforcement record keeps this from computing as pure coordination despite the reading's favorable claim; declaring the real literacy and standardization yield keeps the rupture reading's pure-extraction structure from being attributed to this reading's constraint — that structure belongs to the sibling file, and the omega variables carry the contest over which structure the enforcement record actually matches. The contested founding_problem_status paired with a world_rearranges disappearance verdict records the precise state: the founding campaign is over, the arrangement remains load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the modernization_reading of the orthographic_kernel; does the enforcement record actually match this reading''s structure, or the rupture_reading''s (script change as deliberate cultural severance with the modernization rationale as cover)?',
    'Compare the timing and targeting of enforcement (signage deadlines, publication bans, literacy-course compulsion) against literacy outcomes and against cultural-symbolic targets closed in the same window (dervish lodges, calligraphic institutions, Arabic-script press): enforcement that tracks literacy need supports this reading''s structure; enforcement that tracks cultural severance supports the rupture reading.',
    'Under the rupture structure the same arrangement carries substantially higher epsilon and the victim set widens to the Ottoman textual public; under this reading''s structure the costs are transition prices of a real coordination yield.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Which sibling reading''s structural data the enforcement record matches.').

omega_variable(
    sibling_structure_delta,
    'What structurally changes if a sibling reading is authored instead of this one?',
    'Author the sibling stories as separate files per the epsilon-invariance decomposition rule and let per-seat classifications diverge: the continuity_reading''s constraint has Arabic-script literates and the bearers of the Islamic textual tradition as beneficiaries and this reform''s gainers as victims; the rupture_reading''s constraint lists the Ottoman textual civilization as victim and the new national identity as vindicated proposition. The disagreement is located in the beneficiary/victim declarations and in epsilon, not in the metrics machinery.',
    'Classification of the same historical arrangement differs by reading: this file computes a hybrid with a real coordination function; the rupture reading computes a predominantly extractive one; the continuity reading computes a loss ledger with the script change itself as the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_structure_delta, conceptual, 'Structural delta across sibling readings of the orthographic kernel.').

omega_variable(
    arabic_script_reform_counterfactual,
    'Was full replacement necessary for the literacy and alignment gains, or could a vowel-marked Arabic script (as proposed by Hüseyinzade Ali, Celal Nuri, and others before 1928) have delivered most of the coordination at a fraction of the transition cost?',
    'Comparative script-reform analysis: the Azerbaijani 1929 switch, the Persian and Urdu modernization paths under a retained Arabic script, the pre-1928 Ottoman vowel-notation proposals, and reading-acquisition research comparing shallow Latin and modified Arabic orthographies.',
    'If a reformed Arabic script was viable, the transition generation''s costs were not necessary to the coordination benefit and effective extraction rises; if the Arabic script was structurally inadequate for Turkish phonology and modern typography, the costs were the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arabic_script_reform_counterfactual, empirical, 'Necessity of full script replacement for the claimed coordination yield.').

omega_variable(
    literacy_gain_attribution,
    'How much of the measured literacy expansion is attributable to the script change rather than to concurrent compulsory schooling, state-building, and economic growth?',
    'Cohort and difference-in-differences analysis across the 1928 boundary, and cross-country comparison with Arabic-script countries running similar literacy drives in the same decades.',
    'A small script-specific effect would shrink the coordination function this reading claims and raise the extractive share of the arrangement; a large effect would support the reading''s central claim and lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_attribution, empirical, 'Attribution of literacy gains between script change and concurrent modernization.').

omega_variable(
    new_literate_class_position,
    'Is the new literate class a net beneficiary or also a payer — did the literacy the state granted function as a credential that purchased administrative and military labor at suppressed standing?',
    'Wage, status, and mobility trajectories of the first new-script cohorts against the old elite''s trajectory and against contemporaries excluded from schooling.',
    'If the new literates'' gains were substantially captured by the state through conscription and clerical service, their directionality sits nearer the target end than the beneficiary end and the extraction picture concentrates on the state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_literate_class_position, empirical, 'Whether the new literate class captures or transfers the arrangement''s gains.').

omega_variable(
    kernel_codification_framing,
    'Is the orthographic kernel the 1928 statute (formalized, revisable by assembly) or the script commitment as carried by the writing public (practice-borne, with the statute as one artifact)?',
    'Examine whether revision attempts route through the assembly and the Turkish Language Association (statute-first framing) or through shifts in usage that statutes then ratify (practice-first framing); digital-era spelling variation and the Ottoman-script electives are the live test cases.',
    'Under a practice-first framing, authority_grounding shifts toward practice/distributed, the interpretation layer relocates from the state language institutions to the writing public, and digital-era spelling variation counts as kernel drift rather than interpretation-layer absorption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Framing under-determination in what the kernel IS.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 1928, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__modernization_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement_basis(orth_tr_t1928, observed).
narrative_ontology:measurement(orth_tr_t1935, orthographic_kernel__modernization_reading, theater_ratio, 1935, 0.15).
narrative_ontology:measurement_basis(orth_tr_t1935, observed).
narrative_ontology:measurement(orth_tr_t1950, orthographic_kernel__modernization_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(orth_tr_t1950, observed).
narrative_ontology:measurement(orth_tr_t1975, orthographic_kernel__modernization_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement_basis(orth_tr_t1975, observed).
narrative_ontology:measurement(orth_tr_t2000, orthographic_kernel__modernization_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(orth_tr_t2000, observed).
narrative_ontology:measurement(orth_tr_t2026, orthographic_kernel__modernization_reading, theater_ratio, 2026, 0.26).
narrative_ontology:measurement_basis(orth_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__modernization_reading, base_extractiveness, 1928, 0.6).
narrative_ontology:measurement_basis(orth_be_t1928, observed).
narrative_ontology:measurement(orth_be_t1935, orthographic_kernel__modernization_reading, base_extractiveness, 1935, 0.58).
narrative_ontology:measurement_basis(orth_be_t1935, observed).
narrative_ontology:measurement(orth_be_t1950, orthographic_kernel__modernization_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement_basis(orth_be_t1950, observed).
narrative_ontology:measurement(orth_be_t1975, orthographic_kernel__modernization_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement_basis(orth_be_t1975, observed).
narrative_ontology:measurement(orth_be_t2000, orthographic_kernel__modernization_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement_basis(orth_be_t2000, observed).
narrative_ontology:measurement(orth_be_t2026, orthographic_kernel__modernization_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(orth_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__modernization_reading, suppression_requirement, 1928, 0.55).
narrative_ontology:measurement_basis(orth_su_t1928, observed).
narrative_ontology:measurement(orth_su_t1935, orthographic_kernel__modernization_reading, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement_basis(orth_su_t1935, observed).
narrative_ontology:measurement(orth_su_t1950, orthographic_kernel__modernization_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement_basis(orth_su_t1950, observed).
narrative_ontology:measurement(orth_su_t1975, orthographic_kernel__modernization_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement_basis(orth_su_t1975, observed).
narrative_ontology:measurement(orth_su_t2000, orthographic_kernel__modernization_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement_basis(orth_su_t2000, observed).
narrative_ontology:measurement(orth_su_t2026, orthographic_kernel__modernization_reading, suppression_requirement, 2026, 0.18).
narrative_ontology:measurement_basis(orth_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, turkish_language_purification_reform).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, republican_secularization_settlement).

% DUAL FORMULATION NOTE:
% Member of the orthographic_kernel constraint family: continuity_reading, modernization_reading (this file), and rupture_reading are separate constraints authored from one kernel, linked per the epsilon-invariance decomposition rule — the colloquial label 'the script reform' covers structurally distinct claims with different epsilon values, victim sets, and failure modes. This reading's epsilon (~0.45) counts transition costs as prices of a real coordination yield; the continuity_reading's constraint inverts the ledger (the script change itself is the loss); the rupture_reading's constraint raises epsilon and widens victims to the Ottoman textual public. This file links downstream to the language-purification reform (the Latin script made the word-coining program administrable) and to the republican secularization settlement (the same state capacity enforced both).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
