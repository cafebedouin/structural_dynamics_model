% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant (Immutable Commandment Reading)
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   D&C 132 (Joseph Smith, 1843) claims that polygamous marriage is an
 *   eternal, immutable law required for the highest degree of exaltation.
 *   Under the immutable-commandment reading, this is not a temporary
 *   principle or a practice subject to prophetic revision: it is a cosmic
 *   truth about the structure of eternal family and the conditions of godhood
 *   itself. From 1843 until 1890, the Church practiced polygamy as required
 *   obedience. When federal law criminalized the practice, the
 *   immutable-commandment reading created a structural trap: practitioners
 *   faced federal prosecution for obeying eternal law; compliance with
 *   federal law constituted apostasy from revealed truth; the prophet could
 *   not legitimately revise the doctrine without proving the prior revelation
 *   false. This reading persists despite the 1890 Manifesto (which suspended
 *   practice but did not revoke the doctrine), creating ongoing extraction
 *   through institutional inertia, theater (doctrine claimed but not
 *   enforced), and suppression (dissent forbidden). The constraint is CLAIMED
 *   as tangled-rope (coordination function + extraction) and the authored
 *   metrics support that: genuine coordination (exaltation theology,
 *   patriarchal order), asymmetric extraction (women and non-patriarchal men
 *   bear costs), active enforcement (Temple recommends withheld,
 *   excommunication threatened). The measurement series tracks rising theater
 *   from t=0 to t=150 as the gap between stated doctrine and suspended
 *   practice widens, suggesting institutional maintenance is increasingly
 *   performative.
 *
 * KEY AGENTS:
 *   - Living prophet: institutional authority bound by immutability claim, cannot legitimately revise without admitting prior revelation was false
 *   - Patriarchal hierarchy & priesthood authority: beneficiaries of immutability framing (consolidates male authority, binds obedience to institutional interpretation)
 *   - Polygamous practitioners: payers, identity-locked (belief in eternal requirement traps them despite legal jeopardy)
 *   - Women in plural marriage: payers, victims, identity-locked (exaltation fused with acceptance of plural marriage)
 *   - Federal authorities: external constraint creating structural incompatibility (federal law vs. eternal law)
 *   - Dissenting church members: excluded, prevented from voicing doubt that would erode immutability claim
 *   - Prophetic authority structure: institutional self-protection mechanism (revision would prove past revelation false)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.82).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.88).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '35573499-3167-4f5b-b613-c90d5e99954a').
narrative_ontology:cs_kernel_codification('35573499-3167-4f5b-b613-c90d5e99954a', fixed_text).
narrative_ontology:cs_authority_grounding('35573499-3167-4f5b-b613-c90d5e99954a', extraction).
narrative_ontology:cs_interpretation_layer_present('35573499-3167-4f5b-b613-c90d5e99954a').
narrative_ontology:cs_reading_relation('35573499-3167-4f5b-b613-c90d5e99954a', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('35573499-3167-4f5b-b613-c90d5e99954a', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('35573499-3167-4f5b-b613-c90d5e99954a', foundational, polygamy_eternally_immutable).
narrative_ontology:cs_axiom_status(polygamy_eternally_immutable, holdable).
narrative_ontology:cs_axiom_grounding('35573499-3167-4f5b-b613-c90d5e99954a', polygamy_eternally_immutable, deontological).
narrative_ontology:cs_axiom('35573499-3167-4f5b-b613-c90d5e99954a', secondary, prophetic_revision_constitutes_apostasy).
narrative_ontology:cs_axiom_status(prophetic_revision_constitutes_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('35573499-3167-4f5b-b613-c90d5e99954a', prophetic_revision_constitutes_apostasy, deontological).
narrative_ontology:cs_reference_frame('35573499-3167-4f5b-b613-c90d5e99954a', eternal_immutable_covenant_framework).
narrative_ontology:cs_drift_state('35573499-3167-4f5b-b613-c90d5e99954a', post_manifesto_suspension_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('35573499-3167-4f5b-b613-c90d5e99954a', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_hierarchy).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, priesthood_authority_consolidation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamous_practitioners).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_plural_marriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, prophetic_authority_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds institutional authority to interpret and declare doctrine. Under this reading, the prophet cannot legitimately revise D&C 132 because it is presented as immutable divine law — not as prophetic word subject to revision, but as eternal covenant. The prophet's authority is bound by the immutability claim, creating a structural lock: declaring polygamy optional would constitute apostasy from revealed truth, not prophetic adaptation. Administers temple ceremonies and obedience enforcement.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, living_prophet, agenda_setter,
    institutional, biographical, identity_locked, global).

% The institutional patriarchal order benefits from the immutability framing: polygamy as eternal commandment reinforces male authority over family formation, kinship structure, and women's reproductive autonomy. The doctrine legitimates male control of access to exaltation through the sealing covenant. If D&C 132 were revised as mere 'temporary adaptation' rather than eternal law, the patriarchal claim to cosmic authority would erode.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_hierarchy, beneficiary,
    institutional, generational, arbitrage, global).

% The immutability reading concentrates soteriological authority in the priesthood hierarchy: exaltation requires obedience to the prophet's interpretation of eternal law, not to external civil law or individual conscience. This extraction mechanism persists by binding obedience to the claim of immutable truth rather than to pragmatic adaptation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, priesthood_authority_consolidation, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the material and legal costs of polygamous practice under federal criminalization. Under the immutable-commandment reading, they face a forced choice: comply with eternal law and face legal prosecution, or comply with civil law and face spiritual condemnation as apostates. No legitimate exit exists within the framework — the reading permits no revision that would release them without doctrinal betrayal. Their belief that polygamy is eternally required traps them in legal and social jeopardy.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_practitioners, payer,
    powerless, biographical, identity_locked, national).

% Bear costs through fragmented family structure, reduced economic and legal security, and subordination to the patriarch's authority. Under this reading, the eternal commandment frames their participation as required for exaltation — making exit spiritually catastrophic. Their identity as covenant holders and their hope for eternal salvation are fused with acceptance of plural marriage, suppressing exit even when circumstances create material hardship or abuse.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_in_plural_marriage, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, women_in_plural_marriage, payer).

% Enforced laws criminalizing polygamy starting in the 1860s. This external legal pressure collides with the immutable-commandment reading, forcing practitioners to choose between federal law and revealed eternal law. The federal constraint (polygamy is illegal) and the religious constraint (polygamy is eternally required) are structurally incompatible under this reading, creating a martyrdom dynamic where compliance with either law constitutes violation of the other.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% Members who privately doubt the eternal status of polygamy or believe the practice should be abandoned are systematically prevented from voicing dissent. The immutability claim forecloses internal revision discourse — to question it is to question the foundation of the entire revelatory system. They are excluded from decision-making and are incentivized to remain silent or leave.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_church_members, excluded,
    organized, biographical, constrained, national).

% The institutional mechanism that interprets and enforces doctrine. Under the immutable-commandment reading, the authority structure is self-protecting: any revision of D&C 132 would be read as proof the previous prophet was either not truly inspired or that revelation is not immutable — both of which would undermine the legitimacy of all prophetic claims. The immutability reading creates institutional inertia.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, prophetic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, prophetic_authority_structure, beneficiary).

% Historians, religious scholars, and legal analysts observe the constraint from outside. They see the collision between federal law and the immutable-commandment reading as a structural forcing function: either the claim to immutability was false (undermining prophetic authority), or federal law violated fundamental religious liberty (creating a justice problem). The external view sees this reading as untenable under sustained legal and social pressure.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_hierarchy).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the soteriological problem of how exaltation (the highest eternal reward) is to be accessed and transmitted: D&C 132 claims polygamy is the celestial law of marriage that produces eternal increase (procreation) and requires submission to patriarchal authority. For male practitioners, it coordinates access to divine authority; for women, it coordinates participation in an eternal cosmic order.
% TRANSFER_FUNCTION: Transfers reproductive autonomy, economic resources, and decision-making authority from women and lower-ranking men to the patriarch and the priesthood hierarchy. Transfers obedience (spiritual and practical) from civil law to religious law. Transfers legitimacy from individual conscience to institutional doctrinal authority.
% ABSENT_VOICES: Women denied plural marriage, individuals who experienced polygamy as abuse or exploitation, members who silently doubt the doctrine but cannot voice dissent without risking excommunication, civil libertarians and legal scholars arguing polygamy violates women's rights, competing theological traditions arguing exaltation is compatible with monogamy. These voices are structurally excluded from the doctrine's adjudication because the immutability claim forecloses internal revision discourse.
% DISAPPEARANCE_RATIONALE: If the immutable-commandment reading vanished and was replaced by a revision-permitting reading (either the prophetic-override or temporal-accommodation frame), practitioners would no longer face forced apostasy from federal law compliance. The soteriological function would be preserved (exaltation would remain the goal, the priesthood authority structure would remain) but the specific extraction mechanism — the immutability claim that binds obedience to revealed law — would dissolve. The constraint as presently structured could not persist without the immutability framing.
% FOUNDING_PROBLEM: In 1843, Joseph Smith received the revelation recorded as D&C 132, claiming that polygamy is the eternal law of marriage required for the highest degree of exaltation, and that resistance to it constitutes a covenant-breaking sin. The founding problem was: how to establish an alternative marriage law grounded in divine authority rather than civil contract, and how to bind the community to it despite its violation of monogamous social norms.
% FOUNDING_PROBLEM_CORROBORATION: The Church institution attests the founding problem remains live: they argue exaltation doctrine still requires the eternal nature of plural marriage, even though practice is suspended (Manifesto 1890). However, historical scholars, including those from within the Church's own community, attest the founding problem's function has been superseded: the social role of polygamy (securing high fertility in a frontier population, differentiating early Mormon identity, consolidating patriarchal authority) was historically contingent, not eternal. Church-authorized historical work (Saints: The Story of the Church of Jesus Christ in the Latter Days, 2018) acknowledges Joseph Smith was responding to nineteenth-century practice pressures, not solely receiving immutable cosmic law. The claim to immutability is now contested even within the institution's own scholarship.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) and rising through the interval because the immutability claim binds obedience to institutional interpretation rather than to participant consent or external law — once you accept the covenant is eternal and immutable, you have surrendered the exit option of 'the doctrine was wrong.' Suppression is even higher (0.88) because dissent is not merely discouraged but is treated as spiritual betrayal: to question the immutability of D&C 132 is to question the entire revelatory authority structure. Theater rises from 0.18 to 0.61 as the Manifesto (1890, t≈47) is issued and the gap between claimed doctrine and suspended practice creates an increasingly performative maintenance. The institutional authority structure is locked by the immutability claim — the living prophet cannot revise without confessing the prior prophet's revelation was false, which would destroy the foundation of authority itself. This is the distinctive feature of the immutable-commandment reading: it creates a form of institutional inertia where the constraint persists not because participants want it, but because it has become impossible to revise without institutional collapse.
 *
 * PERSPECTIVAL GAP:
 *   Institutional leaders (agenda-setter, beneficiaries) experience this reading as preserving cosmic truth and institutional authority. They see the Manifesto as pragmatic suspension, not doctrinal revision, because the immutability claim remains logically intact. Practitioners experience it as a trap: they were taught the principle is eternal and required for exaltation, and now face legal jeopardy for obeying what they were told is divine law. Federal authorities experience it as a conflict between two incompatible legal orders. The engine should compute sharply divergent per-seat types: from the beneficiary seat (prophet/hierarchy), the constraint may compute as rope (genuine coordination, voluntarily sustained); from the payer seat (practitioners), it should compute as snare (immutable claim forecloses exit, institutional enforcement suppresses dissent). The measurement series reflects this rising theater: as the practice becomes suspended but doctrine remains claimed, the constraint increasingly becomes performance — maintaining the appearance of cosmic truth while the functional basis erodes.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (patriarch, priesthood, institution) derive d near 0.0 (full subsidy: they extract authority, consolidate power, are not subject to the constraint's costs). The victims (women, non-patriarchal men, practitioners facing legal jeopardy) derive d near 1.0 (full target: they bear legal, social, and psychological costs, their exit is identity-locked because exaltation is fused with acceptance). The living prophet has a paradoxical position: role=agenda_setter (controls doctrine), but exit=identity_locked (cannot revise without admitting error, which would destroy the identity as prophet). The federal authorities (role=observer) create external pressure that collides with the constraint's internal logic, but their external status prevents them from resolving it within this reading's framework. The temporal arc shows rising d for practitioners as the legal pressure increases (initially d might have been lower, accepting the doctrine voluntarily; by t=47 onward, federal criminalization makes their position clearly targeted).
 *
 * MANDATROPHY ANALYSIS:
 *   This is a case where the founding problem (establishing D&C 132 as eternal law binding the community to polygamous practice) persists as stated doctrine but its functional implementation has been suspended. The Manifesto (1890) claimed to suspend practice without renouncing doctrine, preserving the immutable-commandment reading in theory while denying it in practice. This is mandatrophy: the founding problem's function is no longer live (the institution no longer enforces polygamous practice, has not for 130+ years), but the doctrinal commitment to immutability persists as institutional inertia and theological performance. The measurement of theater_ratio rising to 0.61 by t=150 confirms mandatrophy: the constraint persists through theater (maintenance of doctrine without enforcement) rather than through actual functional necessity. The base_properties.mandatrophy_resolved field is FALSE because the institution continues to claim D&C 132 as eternally binding doctrine, even though practice is suspended. Resolution would require either (a) explicit doctrinal revision (the prophetic-override reading), (b) reframing suspension as accommodation not immutability (the temporal-accommodation reading), or (c) institutional collapse of the immutability claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_claim_falsifiability,
    'Is the immutability claim a deontological commitment (intrinsic to the covenant''s moral status) or an empirical claim about the cosmic order (subject to falsification by divine revelation of a superseding commandment)?',
    'Inspection of doctrinal literature distinguishing eternal truths from eternal principles subject to prophetic revision. If the Church''s own theology permits prophetic override, the immutability claim is rhetorical rather than structural — a choice by leaders to foreclose revision, not a logical necessity of revelation.',
    'If empirical/falsifiable, then a new prophetic declaration revising polygamy doctrine would be logically coherent (the prophetic-override reading). If deontological, any revision constitutes apostasy. The reading''s internal coherence depends on which grounding-type is assigned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutability_claim_falsifiability, conceptual, 'Whether immutability is built into the claim''s structure or is a rhetorical frame around a revisable doctrine.').

omega_variable(
    federal_pressure_as_externality_vs_test,
    'Is federal criminalization of polygamy an external constraint that forces the institution to choose (read as prophetic-override or temporal-accommodation pressure), or is federal pressure itself part of the testing mechanism through which the immutable law is proven (read as trials of faith)?',
    'Comparison of institutional rhetoric: if the Manifesto is framed as ''preservation of the institution to continue the work'' (external pressure interpretation), federal pressure is exogenous; if framed as ''trial of faith and obedience to law of the land as higher test'' (internal interpretation), pressure is reframed as providential. Empirical content: did the institution''s doctrine-revision decisions respond to demonstrable legal jeopardy, or would the decisions have occurred on purely theological grounds?',
    'If external, federal pressure becomes a forcing function that exposes the immutable-commandment reading as untenable (it cannot survive real-world conflict with sovereign law). If internal, the immutable-commandment reading absorbs federal pressure as a test, preserving its logical coherence at the cost of higher suppression and theater ratios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_as_externality_vs_test, empirical, 'Whether federal pressure is an external constraint or an internal testing mechanism within this reading''s framework.').

omega_variable(
    suppression_structural_vs_internalized,
    'Does the suppression of dissent (high suppression_requirement = 0.88) arise from institutional enforcement (excommunication threats, reputation damage, leadership pressure) or from internalized fusion of identity with doctrinal obedience (practitioners cannot voice doubt without experiencing themselves as spiritually defective)?',
    'Post-exit trajectories of former members: if suppression persists after institutional exit (continued identity as apostate, internalized shame, cognitive patterns from childhood socialization), the mechanism is substantially internalized. If suppression drops sharply once institutional enforcement is removed, the mechanism is structural.',
    'If internalized, the constraint''s effective suppression on targets is higher than the scalar 0.88 suggests — the targets carry suppression with them even after institutional exit, making the constraint''s hold more persistent. If structural, institutional reform or legal intervention could reduce suppression rapidly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structurally enforced or internalized through identity fusion.').

omega_variable(
    kernel_reading_alternative_framing,
    'This constraint instantiates the immutable-commandment reading of the eternal-marriage-covenant kernel. Could the same D&C 132 revelation be read under the prophetic-override framework (the revelation is immutable IF the living prophet interprets it, but the prophet can declare it superseded) or temporal-accommodation framework (the revelation is eternally true but practice is not eternally required)?',
    'Exegetical analysis of D&C 132 itself: the text does not explicitly foreclose revision, prophetic override, or suspension. Different readings foreground different semantic zones (Joseph Smith''s own statements about ''eternal and immutable,'' the later institutional doctrine of continuing revelation, the Manifesto''s claim to suspend practice while retaining doctrine). No single reading is textually mandatory.',
    'If alternative readings are structurally plausible from the same kernel, the immutable-commandment reading is a CHOICE by institutional actors to foreclose revision, not a logical consequence of the revelation. This reading is one positioning in a contested space, not the only coherent reading. The institutional commitment to this specific reading (over prophetic-override or accommodation) is what drives the high extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether the immutable-commandment reading is the unique coherent reading of D&C 132, or one choice among plausible alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(eter_tr_t47, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 47, 0.48).
narrative_ontology:measurement(eter_tr_t75, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 75, 0.58).
narrative_ontology:measurement(eter_tr_t100, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 100, 0.61).
narrative_ontology:measurement(eter_tr_t150, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 150, 0.61).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(eter_be_t47, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 47, 0.79).
narrative_ontology:measurement(eter_be_t75, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 75, 0.81).
narrative_ontology:measurement(eter_be_t100, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement(eter_be_t150, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 150, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(eter_su_t47, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 47, 0.85).
narrative_ontology:measurement(eter_su_t75, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 75, 0.87).
narrative_ontology:measurement(eter_su_t100, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(eter_su_t150, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 150, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, federal_polygamy_criminalization_constraint).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, priesthood_authority_legitimacy_doctrine).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel has been decomposed into three constraint stories, one per reading. Each reading instantiates a different ε value and beneficiary/victim structure from the same textual base (D&C 132). The immutable_commandment_reading (this story) treats the doctrine as unchangeable cosmic law; the prophetic_override_reading treats it as subject to living revelation; the temporal_accommodation_reading treats it as eternally true but practice-contingent. These are not three measurements of the same constraint — they are three structurally distinct constraints, each corresponding to a coherent but incompatible interpretation of D&C 132. All three constrain the same kernel text, but each derives different ε, different beneficiaries/victims, different persistence mechanisms. They are linked via network.affects_constraints because a shift in how the kernel is read reshapes the constraint landscape — institutional adoption of the prophetic-override reading would dissolve this constraint and activate the override variant instead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
