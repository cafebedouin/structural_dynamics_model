% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus — Abolitionist Reading (No Legitimate Authority Remains)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the Dharmasastra
 *   kernel: the corpus and the varna/jati hierarchy it codifies are
 *   fundamentally and irredeemably extractive, no interpretive move
 *   (literalist or reformist) can restore legitimate authority to it, and the
 *   only coherent response is wholesale abandonment of both the textual
 *   framework and the caste system it underwrites. Under this reading, formal
 *   legal abolition of untouchability and caste discrimination (mid-20th
 *   century constitutional reform) reduced overt state enforcement but did
 *   not eliminate the informal, custom-carried enforcement mechanisms —
 *   kinship sanction, marriage-market exclusion, occupational stigma, temple
 *   access restriction — that continue to draw legitimacy from the corpus's
 *   residual cultural authority. The theater_ratio rises over the interval
 *   because formal caste hierarchy is increasingly defended through symbolic,
 *   'merely spiritual' or 'merely cultural' reframing rather than explicit
 *   legal or theological assertion — a shift toward performative maintenance
 *   the abolitionist reading treats as evasion, not resolution. This is the
 *   abolitionist reading only: the orthodox_literalist reading (eternal
 *   revealed truth) and the reformist_contextual reading (separable ethical
 *   core) are separate constraints with their own ε values, sharing the same
 *   kernel text but authoring entirely different beneficiary/victim
 *   structures and different classifications.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: primary beneficiary and historical interpretive authority (institutional/arbitrage)
 *   - dominant_caste_landholders: secondary beneficiary, local enforcement power (powerful/mobile)
 *   - temple_and_matha_institutions: institutional beneficiary drawing ritual legitimacy from the corpus (institutional/arbitrage)
 *   - dalit_communities: primary victim, structurally excluded and historically placed outside the varna order (powerless/trapped)
 *   - shudra_laborers: victim, assigned subordinate role within the hierarchy (powerless/constrained)
 *   - intercaste_couples: victim of endogamy enforcement (powerless/trapped)
 *   - women_under_caste_marriage_norms: victim of stridharma inheritance and marriage restriction (powerless/constrained)
 *   - abolitionist_reform_movements: excluded voice historically barred from interpretive authority over the corpus (organized/mobile)
 *   - constitutional_and_civil_courts: analytical observer adjudicating the equality/custom boundary (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.88).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.85).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus — Abolitionist Reading (No Legitimate Authority Remains)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '3e531ff2-ceaa-49bc-b5c2-3fd323dc686a').
narrative_ontology:cs_kernel_codification('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', fixed_text).
narrative_ontology:cs_authority_grounding('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', lineage).
narrative_ontology:cs_interpretation_layer_present('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a').
narrative_ontology:cs_reading_relation('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', dharmasastra_corpus__reformist_contextual, influences).
narrative_ontology:cs_axiom('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', foundational, textual_corpus_possesses_zero_residual_authority).
narrative_ontology:cs_axiom_status(textual_corpus_possesses_zero_residual_authority, holdable).
narrative_ontology:cs_axiom_grounding('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', textual_corpus_possesses_zero_residual_authority, deontological).
narrative_ontology:cs_axiom('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', foundational, hierarchy_and_ethical_core_are_inseparable).
narrative_ontology:cs_axiom_status(hierarchy_and_ethical_core_are_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', hierarchy_and_ethical_core_are_inseparable, conventional).
narrative_ontology:cs_reference_frame('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', revealed_hierarchical_order).
narrative_ontology:cs_drift_state('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', post_constitutional_abolition_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('3e531ff2-ceaa-49bc-b5c2-3fd323dc686a', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, temple_and_matha_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, intercaste_couples).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_under_caste_marriage_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically the interpretive custodians of the Dharmasastra texts, occupying the top of the varna hierarchy the texts codify. Continues to draw ritual authority, temple administration roles, and social deference from claims of textual sanction, even where formal legal caste privilege has been abolished by the state. Can reframe or selectively cite the texts to preserve status while facing minimal personal cost from any single verse's disputed status.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class, agenda_setter).

% Hold land, employment gatekeeping, and local political power that the caste hierarchy the Dharmasastra sanctifies helps naturalize and defend. Invoke tradition and social order rhetoric to resist land reform, intercaste marriage, and labor mobility for lower-caste communities. Can relocate capital and political alliances if any single justification collapses.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders, beneficiary,
    powerful, generational, mobile, regional).

% Institutional bodies whose endowments, ritual monopolies, and administrative authority are partly legitimated by claims of continuity with Dharmasastric prescription. Reframe contested provisions as symbolic or already-reformed while preserving underlying entry restrictions and hierarchical ritual roles.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, temple_and_matha_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Historically classified outside or at the bottom of the varna order under Dharmasastric categorization; bear the accumulated weight of untouchability practices, exclusion from temple entry, occupational stigma, and violence justified by appeal to inherited textual and customary order. Formal constitutional abolition of untouchability has not eliminated social enforcement, which persists through custom, kinship sanction, and local power structures that cite the same textual tradition.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_communities, payer,
    powerless, biographical, trapped, national).

% Assigned a servitude-oriented role within the fourfold varna scheme; bear historically constrained access to education, priesthood, and property alongside residual social subordination. Migration to cities offers partial exit but caste identity and social sanction often follow through kinship networks and marriage markets.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_laborers, payer,
    powerless, generational, constrained, regional).

% Face social ostracism, family violence including honor killings, and community sanction for violating endogamy norms the Dharmasastra explicitly codifies as central to maintaining varna purity. State law protects the marriage; it does not reach the informal enforcement that follows from families and caste councils invoking traditional prescription.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, intercaste_couples, payer,
    powerless, immediate, trapped, local).

% Subject to stridharma provisions restricting inheritance, remarriage, and autonomy, framed as necessary to preserving caste-lineage purity and patrilineal property transmission. Exit requires breaking simultaneously with family economic support, caste community standing, and religious sanction — a compounded cost most cannot absorb alone.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_under_caste_marriage_norms, payer,
    powerless, biographical, constrained, national).

% Anti-caste movements (in the lineage of Ambedkarite thought) that argue no partial reform of the Dharmasastra can be legitimate because the hierarchy is the text's organizing logic, not an accretion upon it. Historically marginalized from mainstream religious and legal interpretive authority, their position — that the whole framework requires abandonment rather than reinterpretation — is treated by orthodox and even reformist institutions as outside legitimate theological discourse rather than as a competing reading.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_reform_movements, excluded,
    organized, generational, mobile, national).

% Adjudicate the boundary between constitutionally guaranteed equality and religious/customary practice, formally abolishing untouchability and caste discrimination in law while largely declining to rule on the theological status of the Dharmasastra corpus itself. Their rulings shift enforcement mechanisms without resolving the underlying legitimacy question this reading raises.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, constitutional_and_civil_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading. The abolitionist position holds that the appearance of coordination — social order, ritual continuity, occupational stability — is itself the extraction mechanism's cover story; there is no genuine collective-action problem the hierarchy solves that could not be solved without caste stratification.
% TRANSFER_FUNCTION: Moves social status, ritual privilege, land and occupational access, and freedom from stigma from lower-caste and Dalit communities to upper-caste and priestly groups, transmitted through custom, kinship enforcement, and residual institutional practice rather than exclusively through formal law.
% ABSENT_VOICES: Dalit and anti-caste intellectual traditions have historically been excluded from the seats that produce and interpret the Dharmasastra corpus itself — they were not merely governed by the texts but structurally barred from authoring or reinterpreting them. Their objection is not a dissent within the tradition but a rejection of the tradition's interpretive authority altogether, which is precisely why orthodox and reformist readings alike tend to treat the abolitionist position as external to legitimate exegesis rather than as a stakeholder in it.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra corpus lost all residual social and institutional authority overnight, temple entry restrictions, endogamy enforcement, occupational stigma, and caste-based inheritance rules would lose their primary legitimating reference — beneficiary institutions would need to justify continued practices on other grounds (or abandon them), and Dalit and Shudra communities would gain contestable ground currently foreclosed by appeals to tradition.
% FOUNDING_PROBLEM: The corpus originated (on any reading) to codify social, ritual, and legal order for a stratified agrarian society — assigning roles, resolving disputes, and stabilizing succession and ritual practice across a large and diverse population.
% FOUNDING_PROBLEM_CORROBORATION: Indian constitutional law (Article 17, abolition of untouchability; Article 15, non-discrimination) and Ambedkarite scholarship — sources outside the beneficiary priestly and dominant-caste institutions, and indeed historically opposed by them — attest that whatever ordering function the corpus once served is no longer a legitimate or necessary basis for social organization, and that its persistence past legal abolition is maintained by custom and informal enforcement rather than any live coordination need. No corroboration from outside the beneficiary set attests the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) and suppression comparably high (0.85) because, under this reading, the corpus's function IS the hierarchy — there is no separable coordination residue to net out. Accessibility_collapse is authored moderate-low (0.35) rather than mountain-level, because alternatives to caste-stratified social organization are not merely imaginable but historically achieved (constitutional abolition, inter-caste organizing, urban migration) — the abolitionist claim is precisely that alternatives exist and are actively suppressed, not that none exist. Resistance is authored high (0.78) reflecting over a century of organized anti-caste movement (Phule, Ambedkar, and successor movements) actively contesting the corpus's authority. Theater_ratio rises across the interval as formal legal enforcement recedes and informal/symbolic maintenance increases — a classic Goodhart-style substitution the abolitionist reading reads as evasion rather than genuine reform.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (priestly class, dominant landholders, temple institutions) the corpus and its residual authority may compute as coordination — social order, ritual continuity, occupational stability. From the payer seats the identical structure computes as enforced extraction with no legitimating remainder. This divergence is exactly what the abolitionist reading asserts is real and irreducible — not a matter of perspective to be reconciled but a structural fact about who the arrangement serves.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class, dominant caste landholders, and temple institutions are declared beneficiaries: under this reading they collect real status, ritual, and material advantage from the hierarchy the corpus sanctifies, and their exit options (arbitrage, mobile) reflect that they can reframe or relocate around any single contested provision without losing underlying position. Dalit communities, Shudra laborers, intercaste couples, and women under caste marriage norms are declared victims with trapped or constrained exit — the enforcement mechanisms (kinship sanction, marriage-market exclusion, social ostracism) travel with the person and are not escaped by formal legal change alone, which is why the exit options for these groups are authored as trapped/constrained rather than mobile despite constitutional abolition of untouchability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (codifying order for a stratified agrarian society) is authored as dead per this reading's own account, corroborated by constitutional law and anti-caste scholarship external to the beneficiary set. The disappearance_verdict (world_rearranges) combined with founding_problem_status (dead) is exactly the capture/zombie signature the mismatch-consumer is designed to catch: an arrangement whose original justification has lapsed but whose social and institutional weight persists through inertia, custom, and theatrical reframing (rising theater_ratio) rather than genuine ongoing function. This is not conflated with legitimate coordination — the abolitionist reading explicitly denies any coordination residue exists to preserve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_disambiguation,
    'Is ''the Dharmasastra'' properly understood as a single normative kernel with one determinate content, or as a contested textual/interpretive tradition admitting genuinely incompatible readings (orthodox_literalist, reformist_contextual, abolitionist_rejection) with no neutral adjudicator?',
    'This story treats the label as covering three structurally distinct constraints per the ε-invariance principle: this file instantiates only the abolitionist_rejection reading, authored as ε=0.88 (substantially extractive, no coordination residue). The orthodox_literalist and reformist_contextual readings are separate constraint files with their own ε and stakeholder sets, linked via network.affects_constraints. No single ε is claimed for ''the Dharmasastra'' as an undifferentiated label.',
    'Collapsing the three readings into one constraint would either wash out the abolitionist reading''s high extraction against the reformist reading''s more moderate profile, or overstate extraction for contexts where only the ethical-core reading is operative. Keeping them separate preserves the classification signal proper to each.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_disambiguation, conceptual, 'The kernel decomposes into three incompatible readings; this file is one of them by design, not a compromise position.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if the reformist_contextual reading were adopted instead of this one?',
    'Under reformist_contextual, the victim set persists but is authored as reducible through reinterpretation of time-bound prescriptions while retaining a dharma-as-righteous-conduct ethical core as a legitimate residual authority; beneficiaries would include reform-minded interpretive authorities rather than being wholly displaced. Under this (abolitionist) reading, no residual authority survives reinterpretation and the beneficiary/victim structure is resolved only by dismantling the framework, not reforming it.',
    'If the reformist reading is judged the more defensible structural account, the appropriate classification for the corpus''s operation shifts toward tangled_rope (genuine ethical coordination function coexisting with extractive caste application) rather than snare (pure extraction, no coordination residue) as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Naming the specific structural element (presence vs. absence of a severable ethical core) on which the abolitionist and reformist readings diverge.').

omega_variable(
    orthodox_reading_foreclosure_question,
    'Does the abolitionist_rejection reading logically foreclose the orthodox_literalist reading, or can both remain live within different communities'' frameworks simultaneously?',
    'Examine whether a single legal/theological framework could simultaneously hold ''the hierarchy is revealed eternal truth'' and ''the hierarchy has zero legitimate authority and must be abandoned'' — these appear to be direct contradictories at the level of the corpus''s core legitimacy claim, suggesting a forecloses relation is structurally warranted despite both readings persisting as live positions held by different communities.',
    'If forecloses is correct, no single institution or legal system can coherently adopt both readings at once (though different institutions/communities can each hold one); if coexists_with is judged more accurate, the contradiction is softer than authored and the readings might be reconciled within pluralist legal frameworks (e.g., secular constitutional law bracketing the theological question).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_reading_foreclosure_question, conceptual, 'Whether the abolitionist/orthodox relation is genuine logical foreclosure or merely intense unresolved dispute.').

omega_variable(
    informal_enforcement_persistence,
    'How much of the measured suppression (0.85) is enforced through formal institutional/legal mechanisms versus informal social sanction (family, caste council, marriage-market exclusion) that persists independent of any formal legal status of the corpus?',
    'Field research and legal case data on honor-killing prosecutions, caste-council (khap panchayat) rulings, and temple-entry disputes post-constitutional-abolition would separate the formal-legal suppression component from the informal-customary component.',
    'If suppression is predominantly informal/customary rather than state-enforced, formal legal abolition alone (as courts have provided) cannot resolve the constraint even if the abolitionist reading''s legal claims fully prevail — informal enforcement would need independent remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_enforcement_persistence, empirical, 'Formal-legal versus informal-customary composition of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dhar_tr_t12, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 12, 0.22).
narrative_ontology:measurement(dhar_tr_t24, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 24, 0.34).
narrative_ontology:measurement(dhar_tr_t36, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 36, 0.4).
narrative_ontology:measurement(dhar_tr_t48, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 48, 0.42).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 60, 0.42).
narrative_ontology:measurement(dhar_tr_t70, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.93).
narrative_ontology:measurement(dhar_be_t12, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 12, 0.91).
narrative_ontology:measurement(dhar_be_t24, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 24, 0.88).
narrative_ontology:measurement(dhar_be_t36, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 36, 0.86).
narrative_ontology:measurement(dhar_be_t48, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 48, 0.87).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(dhar_be_t70, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 70, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(dhar_su_t12, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(dhar_su_t24, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(dhar_su_t36, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 36, 0.82).
narrative_ontology:measurement(dhar_su_t48, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 48, 0.85).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(dhar_su_t70, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 70, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dharmasastra_corpus kernel, decomposed per the ε-invariance principle because the natural-language label 'the Dharmasastra' covers structurally incompatible claims about textual authority and hierarchy legitimacy. abolitionist_rejection (this file, ε=0.88, snare) denies any legitimate coordination residue and calls for wholesale abandonment. orthodox_literalist (ε expected high, likely tangled_rope or snare depending on beneficiary/enforcement structure) treats the hierarchy as eternal revealed truth requiring literal observance. reformist_contextual (ε expected moderate, likely tangled_rope) treats the caste prescriptions as severable historical accretion atop a retained ethical core. The three do not share one ε; each is authored independently and linked here for contamination-propagation and family-tracing purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
