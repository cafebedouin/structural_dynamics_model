% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahmin Ritual-Authority Monopoly (Birth-Gated Reading of the Vedic Corpus)
 *   domain: religious authority / social stratification / interpretive legitimacy
 *
 * SUMMARY:
 *   This file authors ONE reading of the vedic_dharmic_corpus kernel: the
 *   hereditary_monopoly_reading, under which ritual and interpretive
 *   authority attach to birth into Brahmin lineages and the varna ordering is
 *   treated as divinely ordained and textually prescribed. Per the
 *   epsilon-invariance principle, the contest between readings is NOT
 *   described inside this constraint; the sibling readings
 *   (bhakti_devotional_reading, reformist_egalitarian_reading) are separate
 *   files linked through network.affects_constraints, and the committer
 *   structure is routed to omega variables. The referent of epsilon is the
 *   standing arrangement under contest - the birth-gated ritual economy as it
 *   actually operates, with its fee flows, endowments, exclusions, and
 *   enforcement - not the reading's endorsed self-description as divine
 *   order. KEY AGENTS (by structural relationship): see key_agents. The
 *   interval spans roughly eighteen centuries (each time unit approximates a
 *   century, ~200 BCE to ~1600 CE), covering the classical codification and
 *   medieval maturation of the arrangement prior to colonial-era disruption.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_lineages: Agenda-setter and collector (institutional / identity_locked) - administers ritual and interpretation, collects the ritual economy, and is fused with the office across generations
 *   - kshatriya_ruling_dynasties: Secondary beneficiary (powerful / constrained) - receives consecration and legitimation, funds the apparatus
 *   - vaishya_merchant_households: Dual payer-beneficiary (moderate / constrained) - funds rites and receives commercial and marital standing
 *   - shudra_laboring_castes: Primary target (powerless / trapped) - bears labor obligations and ritual exclusion, fixed by birth
 *   - dalit_outcaste_communities: Primary target (powerless / trapped) - bears the purity code's heaviest costs with no service return
 *   - women_denied_vedic_access: Primary target (powerless / trapped) - barred from study and officiation across all varnas
 *   - heterodox_shramana_orders: Excluded rival institution (organized / mobile) - maintains alternative ordination lines that the reservation rules exist to police
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.66).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.76).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Ritual-Authority Monopoly (Birth-Gated Reading of the Vedic Corpus)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious authority / social stratification / interpretive legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'b6d6b9cd-3f5b-4042-ac3d-81810acdda2f').
narrative_ontology:cs_kernel_codification('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', fixed_text).
narrative_ontology:cs_authority_grounding('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', lineage).
narrative_ontology:cs_interpretation_layer_present('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f').
narrative_ontology:cs_reading_relation('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', vedic_dharmic_corpus__bhakti_devotional_reading, influences).
narrative_ontology:cs_reading_relation('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', foundational, ritual_adhikara_derives_from_birth).
narrative_ontology:cs_axiom_status(ritual_adhikara_derives_from_birth, holdable).
narrative_ontology:cs_axiom_grounding('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', ritual_adhikara_derives_from_birth, theological).
narrative_ontology:cs_axiom('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', foundational, varna_hierarchy_divinely_prescribed).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_prescribed, holdable).
narrative_ontology:cs_axiom_grounding('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', varna_hierarchy_divinely_prescribed, theological).
narrative_ontology:cs_reference_frame('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', shruti_prescribed_birth_gated_varna_order).
narrative_ontology:cs_drift_state('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b6d6b9cd-3f5b-4042-ac3d-81810acdda2f', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_dynasties).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_laboring_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_outcaste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_denied_vedic_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_merchant_households).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_merchant_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Memorize, transmit, and interpret the Vedic corpus; perform the sacrificial and life-cycle rites on which households, kings, and villages depend; decide who may study which texts and who may officiate. Income arrives as dakshina fees, land grants, and temple endowments tied to ritual service. A man born outside the lineages cannot take up the office, and a man born inside it who abandons it loses rank, marriage standing, and communal place - the office and the person are fused across generations.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, beneficiary).

% Hold military and political power and receive consecration, legitimating genealogies, and ritual counsel from the priestly lineages. The varna ordering places their rule second in sanctity and fixes laborers and merchants below them. Patronizing the arrangement secures their legitimacy; ruling without it exposes them to rivals armed with ritual censure. Some dynasties channeled patronage to rival monastic traditions, but the default path to recognized sovereignty runs through priestly consecration.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_dynasties, beneficiary,
    powerful, generational, constrained, continental).

% Fund rites, festivals, and temple construction, and receive in return recognized standing, auspicious timing, and purification services. They are barred from Vedic recitation and from teaching, so the legitimacy of their wealth depends on offices they may never hold. Stepping outside the ritual framework would strip their transactions and marriages of the recognition that makes them binding.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_merchant_households, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, vaishya_merchant_households, beneficiary).

% Provide agricultural and artisanal labor and ritual offerings while being barred from hearing or studying the Veda on pain of penalty. Their duties and disabilities are fixed by birth and pass to their children. Village ritual life runs through offices they cannot hold, and disputes about their obligations are adjudicated by the same birth-qualified authorities who define those obligations.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_laboring_castes, payer,
    powerless, generational, trapped, continental).

% Sit outside the four-varna order entirely, assigned polluting work and segregated residence, water access, and temple entry. They bear the heaviest costs of the purity code while receiving none of its services. Moving between settlements offers no relief because the same code travels with them and is renewed by the local bearers of ritual authority wherever they settle.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_outcaste_communities, payer,
    powerless, generational, trapped, continental).

% Across varnas, women are barred from Vedic study, initiation, and independent ritual agency. Their religious participation is mediated by fathers, husbands, and sons, and by domestic rites they host but may not officiate. Marriage into a household of the father's choosing is the main sanctioned life path, and ritual standing attaches to obedience within it.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_denied_vedic_access, payer,
    powerless, biographical, trapped, continental).

% Maintain rival monastic institutions with their own ordination lines, open to recruits regardless of birth, and train teachers and disputants outside the priestly guilds. They are barred from Vedic learning by the same rules that reserve it, and they compete for patronage at courts and among merchants. Their existence as functioning alternatives is precisely what the reservation rules police.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, heterodox_shramana_orders, excluded,
    organized, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits a large orally held corpus across generations with verified recitational fidelity; standardizes ritual procedure across a subcontinent of kinship polities; provides a shared normative vocabulary (dharma) for dispute resolution, marriage, inheritance, and life-cycle passage; trains and credentials specialists in recitation, exegesis, and adjudication.
% TRANSFER_FUNCTION: Moves dakshina fees, land grants, temple endowment income, labor obligations, and everyday deference from patrons, merchants, and laboring castes to Brahmin lineages; moves interpretive authority and the right to officiate exclusively to men born into those lineages.
% ABSENT_VOICES: Lower-caste religious seekers, women barred from Vedic study, and rival monastic institutions would object that authority should track training and conduct rather than birth. They are outside the interpretive conversation because the rules of participation - who may learn, speak, and officiate - are themselves what the arrangement enforces.
% DISAPPEARANCE_RATIONALE: If the birth-gated arrangement vanished overnight, the ritual economy (fees, endowments, officiant slots), the credentialing of teachers and judges, village dispute mediation, and the marriage and life-cycle infrastructure would all reorganize around whatever training-and-consent criteria replaced the birth gate; the standing of every varna-ranked household would be renegotiated.
% FOUNDING_PROBLEM: Preserve an orally transmitted ritual corpus with high fidelity in a society without widespread literacy, and provide reliable ritual and normative services - sacrifice, life-cycle rites, lawful adjudication - across scattered agrarian polities.
% FOUNDING_PROBLEM_CORROBORATION: Comparative philology and histories of oral tradition attest that the original transmission problem was real and severe, and epigraphic endowment records corroborate the scale of the ritual economy that grew around it. Attestation that the birth-monopoly mechanism outlived its functional warrant comes from outside the beneficiary set: anti-caste jurists and historians, and the documented success of non-hereditary institutions (monastic schools, later academies) in training ritual specialists. No attestation of the monopoly's continuing necessity exists from any party outside the benefiting lineages.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. Claimed type is tangled_rope because the structure fuses a genuine coordination function (corpus transmission with verified fidelity, ritual standardization, credentialed adjudication) with asymmetric extraction through the same architecture: the birth gate that guarantees transmission continuity is also the gate that reserves office, income, and interpretive authority. Extraction is authored at 0.66 because the transfers (fees, grants, labor, deference) are decoupled from marginal service cost and secured by a criterion - birth - that no performance can satisfy or forfeit. Suppression is higher (0.76) because persistence depends on active machinery: penalties for Veda-hearing below the twice-born line, denial of literacy and schooling to excluded seats, purity segregation enforced through residence, water, and temple access, and adjudication of disputes by the interested authority. Theater ratio (0.41) reflects that a substantial and growing share of ritual activity functions as status maintenance and boundary display rather than substantive service, while transmission and adjudication retain real function - the ratio rises over the interval as ceremonial elaboration outpaces functional need. Accessibility collapse is moderate (0.58): alternatives existed and persisted (rival monastic ordination lines, devotional currents) but operated under material and doctrinal handicap, so alternatives were degraded rather than eliminated. Resistance (0.60) is substantial and recurrent from every excluded seat. Identity-lock dynamics bind the agenda-setter seat: the lock is simultaneously professional (career and training are lineage-bound), relational (marriage and communal standing dissolve on exit), and institutional (the lineage has become the function), so exit is unthinkable without annihilation of standing - breaking that frame would convert the seat's experienced type immediately. Suppression is predominantly structural (sanction, educational exclusion, economic dependency) with a documented internalized residue (purity socialization); the split is carried as an omega. The measurement series run on one shared time grid (all three tracked metrics at all seven points) so no metric row is silently backfilled.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute divergent types from identical structure. From inside the priestly lineages the arrangement is sacred order: duty, mutual obligation, and the only known technology for preserving the corpus - extraction is invisible because the criterion (birth) feels like nature. From the trapped payer seats the same structure operates as enforced dispossession: the criterion that feels like nature from above is experienced from below as a locked door with guards. The dual-positioned merchant seat straddles the gap - it purchases standing from the machine that also refuses it office. The engine computes these per-seat classifications from the structural data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. brahmin_priestly_lineages sit nearest the beneficiary pole (they collect the transfers and administer the rules; identity lock amplifies their investment in the arrangement). kshatriya_ruling_dynasties sit low-mid: they receive legitimation worth more than the endowments they pay. vaishya_merchant_households sit near symmetric - genuine purchased benefit against real exclusion from office. shudra_laboring_castes, dalit_outcaste_communities, and women_denied_vedic_access sit nearest the full-target pole: they pay labor, fees, and deference while receiving no service return, and their trapped exit amplifies effective extraction toward the maximum the engine assigns. heterodox_shramana_orders are excluded rather than coordinated - they sit outside the transfer flow but inside the suppression field, and their mobile exit (functioning alternative institutions) is exactly what distinguishes an excluded rival from a trapped target. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, and the schema's override surface keys on power atoms, which would misapply across the multiple agents sharing each atom here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (oral corpus preservation, ritual standardization, credentialed adjudication) is partially live: transmission and life-cycle services remain real needs, so the arrangement is not a zombie. What is contested is the mechanism - whether the birth gate remains necessary to the function. Classifying the whole arrangement as pure extraction would erase the genuine coordination substrate (and mispredict the world's rearrangement on disappearance); classifying it as pure coordination would erase the birth gate's asymmetry and the enforcement machinery that sustains it. Tangled_rope captures the fusion: coordination and extraction run through the same structure, and active enforcement is load-bearing. founding_problem_status is authored 'contested' (not 'dead'), so the mismatch consumer finds no dead-mandate-plus-world_rearranges zombie flag - correctly, because the underlying problem persists even as the monopoly mechanism is what stands accused. mandatrophy_resolved is therefore not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (hereditary_monopoly_reading) of the vedic_dharmic_corpus kernel; which reading governs a given community''s practice, and how would a sibling reading change the structure?',
    'Observe whose authority claims communities actually honor in ritual disputes, credentialing conflicts, and succession contests; consult the sibling files (bhakti_devotional_reading, reformist_egalitarian_reading), which author the alternative structures directly.',
    'The epsilon value, victim set, and classification here apply only to communities operating under this reading. Under the bhakti reading the birth gate dissolves (different victim set, materially lower extraction); under the reformist reading authority relocates to constitutional-rational critique and the hereditary structure loses its warrant entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: which reading of the corpus governs, and what siblings would change.').

omega_variable(
    divine_ordination_vs_constructed_arrangement,
    'Is the varna hierarchy a natural or divinely ordained order (as this reading asserts), or a constructed arrangement that persists because identifiable agents collect from it?',
    'Comparative-historical analysis of specialist classes in societies lacking birth monopoly: test whether transmission fidelity and ritual reliability track heredity or training investment, and whether the hierarchy''s boundaries move with the beneficiaries'' interests.',
    'If constructed, the natural-law self-description fails and the arrangement stands as enforced extraction riding on a coordination substrate - confirming the tangled_rope structure and blocking any natural-law certification; if the birth gate were shown functionally irreplaceable, part of the measured extraction would reclassify as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordination_vs_constructed_arrangement, empirical, 'Natural-law claim versus constructed-arrangement hypothesis for the varna hierarchy.').

omega_variable(
    transmission_function_separability,
    'Is the corpus-transmission and ritual-standardization function structurally separable from the birth monopoly that currently carries it?',
    'Examine communities where non-hereditary institutions trained ritual specialists (monastic schools open by recruitment, later academies) and compare recitational fidelity, procedural standardization, and adjudication quality against lineage-trained cohorts.',
    'If separable, the extraction measured here is rent on a real coordination function and remedies can remove the gate without losing the function; if inseparable, a portion of epsilon is the price of the coordination itself and excess extraction is correspondingly lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_function_separability, conceptual, 'Whether coordination and extraction components are structurally separable.').

omega_variable(
    suppression_structural_internalized_mix,
    'Is the measured suppression structural (penalties, educational exclusion, economic dependency, segregation enforcement) or partly internalized (purity socialization and caste identity fusion that persists after barriers are removed)?',
    'Cohort studies of families exiting caste-linked occupations and residences: track whether avoidance norms, deference patterns, and status anxiety persist once the enforcing institutions lose reach.',
    'To the extent suppression is internalized, effective suppression exceeds the structural measure and decays slowly after reform - enforcement capacity can fall while experienced constraint remains high, dating any relaxation trajectory later than the institutional record suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_mix, empirical, 'Structural versus internalized share of the suppression carrying the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(vedi_tr_t3, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement(vedi_tr_t6, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(vedi_tr_t9, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(vedi_tr_t12, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(vedi_tr_t18, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 18, 0.41).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(vedi_be_t3, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(vedi_be_t6, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(vedi_be_t9, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 9, 0.63).
narrative_ontology:measurement(vedi_be_t12, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(vedi_be_t18, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 18, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(vedi_su_t3, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(vedi_su_t6, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(vedi_su_t9, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 9, 0.69).
narrative_ontology:measurement(vedi_su_t12, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(vedi_su_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(vedi_su_t18, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 18, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the dharmic tradition' (or 'the caste system') conflates structurally distinct claims and is decomposed per the epsilon-invariance principle. This file authors one family member: the hereditary-monopoly reading of the vedic_dharmic_corpus kernel, with its own epsilon (0.66), beneficiary/victim structure, and enforcement profile. The sibling readings instantiate different constraints from the same corpus with different victim sets and different epsilon values, and are authored separately; family linkage runs through network.affects_constraints. Directionality of the edges: this reading historically supplied the institutional conditions (temple control, ritual economy, credentialing monopoly) under which the sibling readings emerged and had to negotiate, so the influence edges run outward from this file - upstream institutional configuration shaping the downstream challengers' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
