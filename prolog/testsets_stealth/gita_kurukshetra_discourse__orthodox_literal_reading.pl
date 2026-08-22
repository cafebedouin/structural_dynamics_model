% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of the Gita: Birth-Graded Duty and Righteous War
 *   domain: religious/textual-hermeneutic/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita's battlefield discourse is a persisting commitment
 *   about duty and violence; this story instantiates ONE reading of it — the
 *   orthodox literal reading, in which the text mandates birth-graded duty
 *   (svadharma) and sanctions violence in righteous war. As a standing
 *   arrangement the reading coordinates a role-structured social order
 *   (Brahmin teaching, Kshatriya war-fighting, Vaishya trade, Shudra service)
 *   while extracting asymmetrically: lower castes are locked out of mobility
 *   and textual access, and the war sanction converts designated enemies into
 *   legitimate targets whose deaths are absorbed into duty. The claim/metric
 *   gap is deliberate: claimed_type is authored from my structural judgment
 *   (genuine coordination plus enforced asymmetric extraction); the metrics
 *   are authored as descriptive of the reading's actual operation. The
 *   sibling readings (gandhian_allegorical_reading,
 *   universalist_devotional_reading) are separate constraints with their own
 *   epsilon and beneficiary structures — they are linked through the network,
 *   not averaged into this one. KEY AGENTS (by structural relationship):
 *   brahmin_interpretive_class — agenda-setter and principal beneficiary
 *   (institutional/identity_locked); kshatriya_warrior_class — beneficiary
 *   who also bears war risk (powerful/identity_locked);
 *   vaishya_mercantile_class — beneficiary (organized/identity_locked);
 *   lower_castes_shudra_dalit — primary target (powerless/trapped);
 *   designated_enemies_in_dharmic_war — target (moderate/trapped);
 *   anti_caste_reform_movements — excluded voice (organized/mobile);
 *   textual_hermeneutics_scholars — analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.63).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of the Gita: Birth-Graded Duty and Righteous War").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/textual-hermeneutic/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '7c49479d-e4aa-4992-84e9-8725c0579e43').
narrative_ontology:cs_kernel_codification('7c49479d-e4aa-4992-84e9-8725c0579e43', fixed_text).
narrative_ontology:cs_authority_grounding('7c49479d-e4aa-4992-84e9-8725c0579e43', lineage).
narrative_ontology:cs_interpretation_layer_present('7c49479d-e4aa-4992-84e9-8725c0579e43').
narrative_ontology:cs_reading_relation('7c49479d-e4aa-4992-84e9-8725c0579e43', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('7c49479d-e4aa-4992-84e9-8725c0579e43', gita_kurukshetra_discourse__universalist_devotional_reading, forecloses).
narrative_ontology:cs_axiom('7c49479d-e4aa-4992-84e9-8725c0579e43', foundational, svadharma_birth_graded_binding).
narrative_ontology:cs_axiom_status(svadharma_birth_graded_binding, holdable).
narrative_ontology:cs_axiom_grounding('7c49479d-e4aa-4992-84e9-8725c0579e43', svadharma_birth_graded_binding, theological).
narrative_ontology:cs_axiom('7c49479d-e4aa-4992-84e9-8725c0579e43', foundational, dharmic_war_violence_sanctioned).
narrative_ontology:cs_axiom_status(dharmic_war_violence_sanctioned, holdable).
narrative_ontology:cs_axiom_grounding('7c49479d-e4aa-4992-84e9-8725c0579e43', dharmic_war_violence_sanctioned, theological).
narrative_ontology:cs_axiom('7c49479d-e4aa-4992-84e9-8725c0579e43', secondary, brahmin_exclusive_interpretive_authority).
narrative_ontology:cs_axiom_status(brahmin_exclusive_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7c49479d-e4aa-4992-84e9-8725c0579e43', brahmin_exclusive_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('7c49479d-e4aa-4992-84e9-8725c0579e43', varnashrama_divine_ordinance).
narrative_ontology:cs_drift_state('7c49479d-e4aa-4992-84e9-8725c0579e43', constitutional_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c49479d-e4aa-4992-84e9-8725c0579e43', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_mercantile_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_dalit).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, designated_enemies_in_dharmic_war).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, svadharma_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_divine_ordinance).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, nishkama_karma_absolution).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, dharmic_war_conduct_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach, recite, and interpret the text; decide which readings count as authoritative; historically controlled Vedic education and ritual services, collecting fees, land grants, and deference as the price of access. The role is inherited, and the authority structure it administers is what constitutes Brahmin standing — leaving it would mean renouncing the source of the class's identity and livelihood.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, beneficiary).

% Born into the warrior role. The reading assigns them war-fighting as binding duty and returns honor, rule, and absolution from the moral cost of killing. They also bear the personal risk of the wars the reading sanctions — the discourse itself is addressed to a warrior trying to refuse. Abandoning the role is framed as dereliction with spiritual consequence, not a career change.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, generational, identity_locked, continental).

% Assigned trade and agriculture; receives an ordered economic niche and the standing of the twice-born. Bound to the role by the same birth-graded duty structure that assigns the warrior his war and the laborer his service.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_mercantile_class, beneficiary,
    organized, generational, identity_locked, continental).

% Assigned service and labor; historically denied Vedic education, ritual access, and mobility; bear the hierarchy's material and status costs generation after generation. Position is ascribed at birth. Within the reading's own terms there is no legitimate exit — renunciation or conversion carries total social cost, and organized refusal is classified as disorder.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_dalit, payer,
    powerless, generational, trapped, continental).

% Those on the other side of a war the reading classifies as righteous. The war sanction converts them from persons owed restraint into legitimate targets; their deaths are absorbed into the warrior's duty rather than mourned as wrongs. They do not choose this position and cannot decline it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, designated_enemies_in_dharmic_war, payer,
    moderate, biographical, trapped, regional).

% Organized movements from Phule and Ambedkar to contemporary Dalit activism that reject birth-graded duty and the interpretive monopoly outright. They are not seated in the orthodox interpretive conversation, which decides who may read and what counts. Their exit is real and has been exercised: mass conversion movements have left the tradition entirely.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, anti_caste_reform_movements, excluded,
    organized, generational, mobile, national).

% Academic philologists and historians of religion who study the text's composition, its dharma-shastra context, and the history of its interpretations. They hold no seat in the tradition, collect nothing from its operation, and can see the whole structure from outside.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, textual_hermeneutics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates a role-structured social order: it assigns occupation and obligation by birth, resolves the warrior's crisis of conscience by subordinating attachment to duty, and supplies conduct rules for organized war (who may be fought, when, and how). Stated without evaluation: it tells each party what to do, and makes collective action — including organized violence — possible under a shared normative frame.
% TRANSFER_FUNCTION: Moves interpretive authority, status, and ritual income upward to the Brahmin seat and legitimated rule to the warrior seat; moves labor and service obligation upward from the lower castes; and moves the moral cost of killing off the warrior and onto the cosmic order — the act is reframed as duty, the dead as already slain by divine decree.
% ABSENT_VOICES: The lower castes the hierarchy binds have no seat in the interpretive conversation that defines their duty; the slain of righteous war cannot speak; adherents of rival readings are ruled out by the monopoly that decides which reading counts. Anti-caste movements speak from outside the frame and are classified within it as disorder rather than objection.
% DISAPPEARANCE_RATIONALE: If the reading's authority vanished overnight, the birth-graded duty structure loses its divine warrant: caste practice loses its legitimating frame (as it partly has under constitutional law), the warrior's refusal-to-fight crisis loses its prescribed resolution, and the interpretive monopoly's income and standing collapse. Households, temples, and schools organized around the reading would rearrange — the rearrangement is already visible wherever the reading's enforcement has decayed.
% FOUNDING_PROBLEM: A warrior on the field refuses to fight his own kin; a role-structured society needs a way to assign duty, legitimate its defense, and absorb the moral cost of necessary violence without dissolving the social order that requires it.
% FOUNDING_PROBLEM_CORROBORATION: The immediate crisis is attested from outside the beneficiary seats by the epic's own dramatic structure and by academic philology, which date the text to a war-culture context in which the warrior's dilemma was real. Orthodox commentators attest the general problem (acting under moral paralysis) as live; anti-caste scholarship — Ambedkar's critique of the Gita's defense of caste is the canonical instance — and constitutional history attest the caste-mandate function as superseded in public law. No single outside source corroborates the whole genealogy; the split in the corroboration itself is the signal.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the reading's costs are birth-ascribed and one-directional: labor, service, exclusion, and war deaths flow to seats that never agreed to them, while authority and absolution flow to seats that administer the arrangement. Suppression (0.63) is substantial but has decayed: the interval models roughly a century (time point 0 approx. 1920 to time point 100 approx. 2020) in which constitutional prohibition of untouchability, anti-discrimination law, and atrocity legislation dismantled the formal enforcement machinery — hence the falling suppression_requirement series, which is authored precisely because this story tracks enforcement-capacity decay rather than a static enforcement picture. Theater_ratio rises (0.18 to 0.36) as operative enforcement contracts: recitation, Gita-jayanti observance, and symbolic invocation increasingly carry the reading's maintenance while its operative caste-duty enforcement persists mainly in community-level social sanction. All three series run on one shared time grid {0, 20, 40, 60, 80, 100} so every metric is authored at every examined point. Accessibility_collapse (0.65) is high-within-frame but not total: once the text's authority is accepted, alternatives to one's assigned duty collapse almost completely inside the reading (abandonment is framed as spiritual ruin), yet framework-level exits — rival readings, conversion — remain real, which is what keeps the value below mountain-grade. Resistance (0.6) reflects a century of organized refusal: Bhakti-era antecedents, Phule and Ambedkar, mass conversion, and constitutional rejection. Coalition note: the powerless payer seat's resistance historically materialized as coalition (organized anti-caste movements), which is the main reason the enforcement trajectory bends downward.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administering seat compute differently. From the Brahmin seat the reading is the cosmic order itself — duty, hierarchy, and interpretive authority are one fabric, and the seat is identity-locked: the authority structure constitutes Brahmin standing, so exit is identity dissolution, not relocation. From the Kshatriya seat the reading is dual: it grants legitimation and absolution but also commands the seat into the path of the sanctioned wars — a beneficiary who pays in risk. From the lower-caste seat the same structure is an inherited enclosure with no legitimate door. If the identity frame broke at the administering seat (mass renunciation of the monopoly, as conversion movements partially forced), the enforcement coalition would shrink faster than the doctrine could replace it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The Brahmin seat sits near the full-beneficiary end (d near 0.05): it collects interpretive rents, ritual income, and deference while administering the rules. The Vaishya seat is a low-d beneficiary (ordered niche, twice-born standing). The lower-caste seat sits near the full-target end (d near 0.9): birth-ascribed costs, no exit, and the reading's own terms classify its refusal as disorder. The designated-enemy seat sits nearest the target end (d near 0.95): being converted into a legitimate target is not a position one exits by choice. One override is authored: power_atom 'powerful' to d 0.30. The derivation reading only the beneficiaries[] declaration would place the Kshatriya seat near 0.05-0.1; but that seat also bears the wars' personal risk and the discourse's moral burden — the text is addressed to a warrior trying to refuse — so the true structural relationship sits at 0.3, not full beneficiary. The Kshatriya seat is the only 'powerful' seat in this story, so the override is effectively seat-specific. Excluded and observer seats sit outside the flow and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification has to hold two things at once, which is why the claim is tangled_rope rather than snare or rope. Reading the arrangement as pure extraction erases the genuine coordination it performs: role assignment for a complex division of labor, conduct rules that bound war rather than unleash it, and a workable resolution for the crisis of conscience the discourse dramatizes. Reading it as pure coordination erases the asymmetric, enforced extraction: the birth lock, the interpretive monopoly, and the sanctioned dead. The mandatrophy question — has the mandate outlived its function — is genuinely contested rather than resolved: in constitutional public law the caste mandate is dead; in orthodox communities it is live. The measurement pattern (enforcement decay with rising theater and persistent extraction) is consistent with a constraint in mandatrophy transition, but the transition is incomplete and regionally uneven, so no resolved flag is declared and founding_problem_status is authored as contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story is one reading of the gita_kurukshetra_discourse kernel; what would the structural delta be if a sibling reading were adopted instead?',
    'Author the sibling stories (gandhian_allegorical_reading, universalist_devotional_reading) and compare computed classifications across the family; do not re-measure this story under a sibling''s assumptions.',
    'Under the gandhian reading the beneficiary set empties of castes and the war victims vanish (violence is internal, not physical); under the universalist reading caste-graded access dissolves entirely and the type computation should move toward low-extraction coordination. Cross-reading comparison, not within-reading adjustment, is the resolution path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which kernel, which reading, and what the sibling readings would change structurally.').

omega_variable(
    varna_guna_karma_hermeneutic,
    'Is the text''s varna reference a birth-fixed hierarchy (the orthodox reading) or a quality-and-action-based order (guna and karma, per 4.13) that later tradition rigidified?',
    'Philological and commentarial-historical analysis of 4.13 against 18.41-44 and the dharma-shastra reception history; trace when birth-fixity became the operative reading.',
    'If quality-based, the divine-ordinance-of-birth-hierarchy claim weakens, the beneficiary structure contracts to the later tradition''s enforcers, and the reading''s extraction falls; if birth-fixed, the orthodox reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_guna_karma_hermeneutic, conceptual, 'Whether the hierarchy the reading enforces is textual or a reception-layer construction.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the reading''s current suppression structural (jati enforcement: marriage, dining, economic exclusion) or internalized (duty-as-identity, fear of adharma)?',
    'Post-exit trajectory of converts and renunciants: if duty-anxiety and hierarchy deference persist after structural enforcement is escaped, the internalized share is substantial.',
    'If largely internalized, formal legal equality understates the arrangement''s operative force — suppression carries with the agent after exit, and the falling suppression series overstates the decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the reading''s enforcement.').

omega_variable(
    war_legitimation_dormancy,
    'Is the righteous-violence component currently operative (invoked in communal or nationalist mobilization) or dormant (maintained ritually, not applied)?',
    'Track invocations of dharmic-war framing in contemporary political rhetoric and violence; distinguish ritual recitation and textual celebration from operative sanction of specific acts.',
    'If operative, the victim set includes present-day dead and the current extractiveness understates extraction; if dormant, the component is maintained theatrically and theater_ratio should be authored higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_legitimation_dormancy, empirical, 'Whether the war sanction is a live enforcement instrument or a maintained artifact.').

omega_variable(
    coordination_vs_hierarchy_cover,
    'Does the reading''s role-assignment perform a coordination function that would survive without birth-grading, or is the coordination story cover for hierarchy maintenance?',
    'Compare societies with functionally equivalent role coordination (division of labor, bounded war conduct, crisis-resolution frameworks) that do not grade duty by birth; if coordination survives without birth-grading, the extraction is separable from the coordination.',
    'If separable, the reading''s costs are not the price of its coordination and the extraction side of the computation should weight more heavily; if inseparable, part of the measured cost is coordination cost proper.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_hierarchy_cover, conceptual, 'Whether the coordination function and the birth-graded extraction are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 100, 0.36).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 80, 0.73).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 80, 0.66).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 100, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what the Gita teaches about war and caste' covers three structurally distinct constraints — one per reading of the shared kernel. This story authors the orthodox literal reading only: epsilon is authored for the literal-mandate arrangement as this reading holds it, never averaged across readings. The siblings carry their own beneficiary/victim structures (under both, the caste beneficiaries empty out and the war victims vanish); the family is linked so cross-reading comparison stays possible without contaminating any single reading's epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
