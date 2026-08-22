% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Reading of the Dharmasastra Varna Order
 *   domain: religious law/textual interpretation/normative authority
 *
 * SUMMARY:
 *   The orthodox literalist reading holds the Dharmasastra corpus — centrally
 *   the varna/jati prescriptions — to be eternal, revealed, and literally
 *   binding. This story models the standing arrangement those prescriptions
 *   govern: a continent-spanning ritual-legal hierarchy in which occupation,
 *   marriage, ritual access, and legal capacity are fixed by birth;
 *   administered by a priestly interpreter class; enforced through royal
 *   patronage and social sanction; and moving labor, service, ritual fees,
 *   education, and status from Shudra, Dalit, and female populations toward
 *   upper-caste benefit. Per the committer frame, this file instantiates ONLY
 *   the orthodox reading of the dharmasastra_corpus kernel; the
 *   reformist-contextual and abolitionist-rejection readings are separate
 *   constraints with their own epsilon values and victim sets, linked through
 *   the network. The claim/metric gap is deliberate and load-bearing: the
 *   reading CLAIMS mountain (eternal revealed truth, hence
 *   emerges_naturally), while the authored metrics describe substantially
 *   extractive, actively enforced operation. With beneficiaries declared on a
 *   mountain claim, the false-summit machinery evaluates exactly this
 *   divergence — the engine, not the claim, adjudicates.
 *
 * KEY AGENTS:
 *   - - brahmin_priesthood: Agenda-setter and principal beneficiary (institutional/identity_locked) — interprets, administers, and collects through the corpus it transmits
 *   - - kshatriya_ruling_lineages: Co-beneficiary and enforcement arm (institutional/constrained) — trades protection and revenue for ritual legitimation
 *   - - vaishya_commerce_castes: Secondary beneficiary (organized/constrained) — purchases ranked respect and closure from below
 *   - - shudra_service_castes: Primary target (powerless/trapped) — hereditary labor and service, barred from scriptural study
 *   - - dalit_outcaste_communities: Most extreme target (powerless/trapped) — placed outside the varna order entirely, segregated and sanction-exposed
 *   - - caste_women_under_patrilineal_control: Cross-cutting target with partial beneficiary position (moderate/identity_locked) — barred from Vedic agency, bound by pativrata duty, yet sharing caste rank
 *   - - heterodox_anti_caste_movements: Excluded voice (organized/mobile) — rejects the hierarchy, denied standing in shastric councils
 *   - - modern_constitutional_state: Analytical observer (institutional/analytical) — abolishes untouchability in law and partially counter-enforces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.71).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.66).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.71).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, mountain).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Reading of the Dharmasastra Varna Order").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious law/textual interpretation/normative authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).
domain_priors:emerges_naturally(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, 'f8b4198e-e124-4dab-9fea-758807459611').
narrative_ontology:cs_kernel_codification('f8b4198e-e124-4dab-9fea-758807459611', fixed_text).
narrative_ontology:cs_authority_grounding('f8b4198e-e124-4dab-9fea-758807459611', lineage).
narrative_ontology:cs_interpretation_layer_present('f8b4198e-e124-4dab-9fea-758807459611').
narrative_ontology:cs_reading_relation('f8b4198e-e124-4dab-9fea-758807459611', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('f8b4198e-e124-4dab-9fea-758807459611', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('f8b4198e-e124-4dab-9fea-758807459611', foundational, vedic_injunctions_eternally_binding).
narrative_ontology:cs_axiom_status(vedic_injunctions_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('f8b4198e-e124-4dab-9fea-758807459611', vedic_injunctions_eternally_binding, theological).
narrative_ontology:cs_axiom('f8b4198e-e124-4dab-9fea-758807459611', foundational, varna_duties_divinely_apportioned).
narrative_ontology:cs_axiom_status(varna_duties_divinely_apportioned, holdable).
narrative_ontology:cs_axiom_grounding('f8b4198e-e124-4dab-9fea-758807459611', varna_duties_divinely_apportioned, theological).
narrative_ontology:cs_reference_frame('f8b4198e-e124-4dab-9fea-758807459611', eternal_revealed_varnadharma_order).
narrative_ontology:cs_drift_state('f8b4198e-e124-4dab-9fea-758807459611', contemporary_post_constitutional_india, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f8b4198e-e124-4dab-9fea-758807459611', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_ruling_lineages).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishya_commerce_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_service_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_outcaste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, caste_women_under_patrilineal_control).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, caste_women_under_patrilineal_control).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, apauruseyatva_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, karma_rebirth_theodicy).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, svadharma_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Composes, transmits, and adjudicates the corpus: memorizes and teaches Veda, performs rites for other households, collects dakshina and land-grant income, and rules on purity, penance, and precedence. Its standing, livelihood, and self-conception are constituted by the system it administers; abandoning it forfeits identity and income together.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Holds land, revenue, and armed force; enforces varna duties through courts, patronage, and punishment; receives legitimation, labor, and tribute from the ordered hierarchy. Dynastic survival is tied to priestly endorsement — ruling against the hierarchy risks losing the ritual legitimacy that crowns them.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_ruling_lineages, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, kshatriya_ruling_lineages, agenda_setter).

% Trades and finances under purity-adjacent status: ritually ranked above laboring castes, entitled to initiation and sacraments, shielded from stigmatized occupations. Gains from a closed status ladder that caps competition from below; leaving the ladder would trade ranked respect for anonymous mobility.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishya_commerce_castes, beneficiary,
    organized, generational, constrained, continental).

% Bears hereditary service obligations to landholding and priestly households — field labor, artisanal and domestic service — while barred from Vedic study and initiation. Occupation, marriage, and residence are fixed by birth; village-bound debt and custom leave no practical way out.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_service_castes, payer,
    powerless, generational, trapped, regional).

% Placed outside the varna order entirely: hereditary 'impure' occupations, segregated housing, denial of temple entry, water access, and schooling. Transgression draws sanction from ritual penalty to violence; flight means destitution in an unfamiliar world.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_outcaste_communities, payer,
    powerless, generational, trapped, regional).

% Under this reading, women across varnas are barred from Vedic recitation and independent ritual agency, bound by pativrata duty, early-marriage norms, and widowhood austerity; upper-caste women additionally share their caste's rank, honor, and endogamous protection. Identity fuses with wifely duty; refusal or renunciation costs family, honor, and community at once.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, caste_women_under_patrilineal_control, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, caste_women_under_patrilineal_control, beneficiary).

% Buddhist, Jain, bhakti, Sikh, and later Ambedkarite currents reject birth-ranked hierarchy and build parallel congregations; they are denied standing in shastric councils and branded heretical or impure. Their exit is real — renunciation, conversion, separate community — but each exit is socially punished and leaves the orthodox corpus intact.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, heterodox_anti_caste_movements, excluded,
    organized, generational, mobile, continental).

% Constitutionally abolishes untouchability, guarantees equality, mandates reservations, and prosecutes caste atrocities; it observes and partially counter-enforces against the arrangement without administering it, and its reach thins in rural and ritual domains.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, modern_constitutional_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large agrarian society's division of labor, ritual specialization, and marriage pools: hereditary occupational transmission reproduces skills across generations; endogamy rules stabilize kinship alliance and property transmission; purity boundaries standardize interaction among millions of strangers under pre-modern conditions. Stated without evaluation.
% TRANSFER_FUNCTION: Moves labor, personal service, ritual fees, deference, and access to education and scripture from Shudra, Dalit, and female populations upward to Brahmin and upper-caste elites; nominally returns protection and ritual mediation downward.
% ABSENT_VOICES: Shudras and Dalits had no standing in shastric interpretation — barred from hearing, let alone authoring, the texts that governed them; women were barred from Vedic recitation; Buddhist, Jain, bhakti, and anti-caste voices were excluded from orthodoxy-defining councils. They stood outside the pandit assemblies — in fields, folk practice, and later in print and constitutional politics.
% DISAPPEARANCE_RATIONALE: If the prescription-and-enforcement complex vanished overnight, hereditary occupational closure, endogamy enforcement, ritual monopoly, and purity-based segregation would lose their normative warrant simultaneously; land-labor relations, marriage markets, and village service exchange would renegotiate; the beneficiary seats would lose status positions they cannot reproduce by other means.
% FOUNDING_PROBLEM: Consolidating a vast, heterogeneous agrarian population under a single ritual-legal order: allocating occupational and ritual roles hereditarily, legitimating kingship, and integrating diverse local cults and kinship systems into one Brahmanical framework in the centuries after Vedic composition.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of South Asian state formation and epigraphy (inscriptions recording varna-ordered land grants) corroborates the founding coordination and legitimation functions from outside the beneficiary set; anti-caste intellectual traditions (Phule, Ambedkar) attest the arrangement's extractive character from outside; the orthodox attestation of the problem's permanence is self-interested and corroborated mainly within the beneficiary community itself.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, ExtMetricName, E),
    domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dharmasastra_corpus__orthodox_literalist),
    narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71: the arrangement moves hereditary labor, personal service, ritual dues, and educational/ritual access upward at scale, attenuated from its pre-constitutional peak (0.86 in 1800) by legal abolition and assertion politics, with a post-2000 uptick as vigilante and panchayat enforcement substitutes for withdrawn state enforcement. Suppression (0.66) is authored as a raw structural property — it is NOT scaled by power or scope; the engine scales only extractiveness, by directionality and scope. Theater (0.48) reflects a widening gap between professed orthodoxy and substantive observance as constitutional reality, urbanization, and migration erode practice. Accessibility collapse (0.58): exits exist — conversion, migration, secular employment — but at severe social cost, and the reading denies them legitimacy altogether. Resistance (0.62): two centuries of bhakti, anti-caste, and constitutional challenge. All three series run on one shared grid ({1800, 1860, 1920, 1950, 1980, 2025}); the dynamics are broadly monotonic with a late enforcement-substitution uptick rather than cyclical, so no intermittent-reinforcement reading is asserted.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute very differently. From the brahmin_priesthood seat the arrangement is a cosmic order it stewards and staffs — near-symmetric personal economics fused with custodial identity (institutional identity-lock: the organization has become its function; break the frame and the priesthood loses livelihood and self-conception together). From the dalit_outcaste_communities seat the same structure is enforced extraction with no exit — maximal directionality, trapped, with a partially internalized lock (karma-deserving frames that survive structural exit; see the suppression-mechanism omega). The kshatriya seat experiences legitimation income against enforcement burden; the women's seat splits internally between caste-rank benefit and patriarchal cost. The engine derives these per-seat classifications from the structural data; the authored mountain claim belongs to the reading, not to any seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (priesthood, ruling lineages, merchant castes) drive d toward the beneficiary pole; victim declarations (Shudra, Dalit, women) drive d toward the target pole, amplified by trapped and identity_locked exits — Dalit seats sit nearest the full-target end. One override: caste_women_under_patrilineal_control holds payer with a secondary beneficiary position (shares caste rank and endogamous protection while bearing gender-specific exclusion); the derivation from dual roles would misread the net position, so an explicit override sets d=0.65 at the moderate power atom — the only moderate-power seat in the story, so the override touches no other agent. Gain_flow names brahmin_priesthood: the reading's own transfer architecture routes dakshina, first-claim service, and land-grant corpora (agrahara, brahmadeya) to the priestly seat; ruling lineages receive the larger material share in some periods, but no alternative single seat captures the ritual-fee stream, and 'diffuse' would falsely assert that no capturer exists. Fixing cost is prohibitive: removal requires dismantling an identity infrastructure in which every seat is invested, the beneficiary bloc controls interpretive authority, and the partial fix already tried (constitutional abolition) demonstrably did not complete the job.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating a vast agrarian population under one ritual-legal order and legitimating kingship — has been transformed rather than solved: constitutional law, labor markets, and print politics now perform integration differently. The R5 interview records status=contested (the orthodox attest permanence; historiography and anti-caste traditions attest the ordering function's obsolescence) against verdict=world_rearranges. Because concentrated beneficiaries still actively maintain the arrangement — enforcement substitution is rising, not decaying — the mandatrophy question resolves AWAY from piton: this is maintained capture, not inertial residue. The mismatch consumer reads status x verdict; contested status yields no automatic zombie flag, but the rising theater series marks the same underlying drift the piton test watches for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    varna_naturality_vs_construction,
    'Is the varna/jati hierarchy an eternal, divinely ordained order as this reading claims, or a constructed arrangement whose boundaries track elite interests across regimes?',
    'Comparative-historical analysis: if caste boundaries shift with land-grant patterns, census politics, and dynastic interest rather than tracking any invariant natural kind, the constructed reading wins.',
    'If constructed, the mountain claim is a false summit: with beneficiaries declared, the false-summit path reclassifies toward tangled_rope/snare and the eternal-truth framing functions as cover for identifiable rent collection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_naturality_vs_construction, empirical, 'Whether the hierarchy is natural/divine law or constructed order.').

omega_variable(
    epsilon_reading_index_ambiguity,
    'Does this reading''s valuation of hierarchical flows as duty rather than wrongful taking warrant a lower epsilon than the descriptive magnitude of transferred labor, service, and status supports?',
    'Cross-reading comparison once the sibling stories are authored: if the abolitionist and reformist stories author materially higher epsilon over the same referent, the residual divergence locates in evaluative framing rather than measurement.',
    'Epsilon here is reading-indexed over a fixed referent; a resolved duty-not-taking verdict would pull epsilon down and soften the computed target-side seats, while a rejected verdict confirms the descriptive figure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_reading_index_ambiguity, conceptual, 'Reading-indexed epsilon versus descriptive flow magnitude.').

omega_variable(
    text_vs_enforced_practice_decomposition,
    'Is the operative constraint the textual corpus as scholarship, or the enforced social practice that cites it — and do the two yield different epsilon?',
    'Examine divergence sites: practices without shastric warrant, and warranted practices abandoned in practice; if epsilon differs systematically between text-as-study and text-as-enforced-order, decompose into two linked stories per the epsilon-invariance principle.',
    'If practice dominates, this story''s epsilon measures the enforced arrangement and a separate textual-authority story is required; conflating them would smear both classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_vs_enforced_practice_decomposition, conceptual, 'Boundary between the textual kernel and its enforced social operation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression sustaining observance structural (residential segregation, economic dependency, sanction networks) or internalized (karma-deserving frames, purity self-policing, pativrata identity fusion)?',
    'Post-exit trajectory of converted and migrated communities: if purity self-policing and deference habits persist after structural barriers fall, a substantial internalized component is established.',
    'Internalized suppression travels with the target after exit, raising effective suppression above the structural measure and deepening identity-lock on payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the suppression that sustains observance.').

omega_variable(
    enforcement_substitution_trajectory,
    'Will social and vigilante enforcement continue substituting for withdrawn state enforcement, or decay under generational change?',
    'Longitudinal tracking of khap and honor-violence incidence, inter-caste marriage rates, temple-access incidents, and atrocity prosecutions.',
    'Rising substitution holds suppression high and keeps payer seats on the enforced-extraction side; decay pushes the arrangement toward theatrical maintenance as beneficiaries stop paying enforcement costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_substitution_trajectory, empirical, 'Post-constitutional enforcement trajectory of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1800, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1800, 0.16).
narrative_ontology:measurement_basis(dhar_tr_t1800, observed).
narrative_ontology:measurement(dhar_tr_t1860, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1860, 0.19).
narrative_ontology:measurement_basis(dhar_tr_t1860, observed).
narrative_ontology:measurement(dhar_tr_t1920, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1920, 0.24).
narrative_ontology:measurement_basis(dhar_tr_t1920, observed).
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1950, 0.33).
narrative_ontology:measurement_basis(dhar_tr_t1950, observed).
narrative_ontology:measurement(dhar_tr_t1980, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1980, 0.41).
narrative_ontology:measurement_basis(dhar_tr_t1980, observed).
narrative_ontology:measurement(dhar_tr_t2025, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(dhar_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1800, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1800, 0.86).
narrative_ontology:measurement_basis(dhar_be_t1800, observed).
narrative_ontology:measurement(dhar_be_t1860, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1860, 0.84).
narrative_ontology:measurement_basis(dhar_be_t1860, observed).
narrative_ontology:measurement(dhar_be_t1920, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1920, 0.79).
narrative_ontology:measurement_basis(dhar_be_t1920, observed).
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement_basis(dhar_be_t1950, observed).
narrative_ontology:measurement(dhar_be_t1980, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1980, 0.67).
narrative_ontology:measurement_basis(dhar_be_t1980, observed).
narrative_ontology:measurement(dhar_be_t2025, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2025, 0.71).
narrative_ontology:measurement_basis(dhar_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1800, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1800, 0.82).
narrative_ontology:measurement_basis(dhar_su_t1800, observed).
narrative_ontology:measurement(dhar_su_t1860, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1860, 0.85).
narrative_ontology:measurement_basis(dhar_su_t1860, observed).
narrative_ontology:measurement(dhar_su_t1920, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement_basis(dhar_su_t1920, observed).
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1950, 0.64).
narrative_ontology:measurement_basis(dhar_su_t1950, observed).
narrative_ontology:measurement(dhar_su_t1980, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement_basis(dhar_su_t1980, observed).
narrative_ontology:measurement(dhar_su_t2025, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(dhar_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Dharmasastra authority' decomposes into three readings of one kernel with materially different epsilon and victim sets. This (orthodox_literalist) story authors the enforced-hierarchy arrangement as the reading itself sees it — eternal revealed truth claimed as mountain, with the full victim structure declared. The reformist_contextual sibling authors the same referent with the caste prescriptions discounted as time-bound accretion; the abolitionist_rejection sibling authors it with no legitimate authority surviving. Upstream/downstream: this reading's intransigence is the structural condition the siblings mobilize against, so its edges point at both. Each file links the others via affects_constraints; epsilon values are not comparable across files except as reading-indexed measurements over the shared referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
