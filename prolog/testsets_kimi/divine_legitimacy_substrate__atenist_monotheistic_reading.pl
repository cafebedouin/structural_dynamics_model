% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Pharaonic Monopoly on Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the atenist_monotheistic_reading of the
 *   divine_legitimacy_substrate kernel. During the Amarna period, Akhenaten
 *   declared the solar disc Aten the exclusive deity and himself its sole
 *   legitimate revelatory intermediary. The reform dismantled the economic
 *   and political power of the Amun priesthood, closed rival temples, and
 *   redirected religious practice through the pharaoh alone. The constraint
 *   is authored as a tangled_rope: it carries a genuine coordination function
 *   (ending priestly rivalry and centralizing state cult) while extracting
 *   massively from the old priesthoods and enforcing asymmetric compliance on
 *   the populace. Its sibling readings â amun_polytheistic_reading and
 *   folk_syncretistic_reading â are structurally incompatible and logically
 *   foreclosed by this reading's exclusivity claims.
 *
 * KEY AGENTS:
 *   - pharaonic_court (agenda_setter/institutional/identity_locked) â claims sole revelatory monopoly, fuses pharaonic identity with Atenist doctrine, captures temple wealth
 *   - amun_priesthood (payer/organized/trapped) â loses estates, ritual role, and legitimacy under exclusive Atenism
 *   - general_populace (payer/powerless/constrained) â forced to redirect public worship through pharaoh, traditional practice driven underground
 *   - folk_practitioners (payer/powerless/identity_locked) â local syncretic ritual cycles criminalized, religious identity fused with suppressed household practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Pharaonic Monopoly on Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'd401a056-dd20-4267-9460-dd232dc7bcd7').
narrative_ontology:cs_kernel_codification('d401a056-dd20-4267-9460-dd232dc7bcd7', formalized).
narrative_ontology:cs_authority_grounding('d401a056-dd20-4267-9460-dd232dc7bcd7', extraction).
narrative_ontology:cs_interpretation_layer_present('d401a056-dd20-4267-9460-dd232dc7bcd7').
narrative_ontology:cs_reading_relation('d401a056-dd20-4267-9460-dd232dc7bcd7', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('d401a056-dd20-4267-9460-dd232dc7bcd7', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('d401a056-dd20-4267-9460-dd232dc7bcd7', foundational, aten_exclusive_divinity).
narrative_ontology:cs_axiom_status(aten_exclusive_divinity, holdable).
narrative_ontology:cs_axiom_grounding('d401a056-dd20-4267-9460-dd232dc7bcd7', aten_exclusive_divinity, theological).
narrative_ontology:cs_axiom('d401a056-dd20-4267-9460-dd232dc7bcd7', foundational, pharaoh_sole_revelatory_authority).
narrative_ontology:cs_axiom_status(pharaoh_sole_revelatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('d401a056-dd20-4267-9460-dd232dc7bcd7', pharaoh_sole_revelatory_authority, conventional).
narrative_ontology:cs_reference_frame('d401a056-dd20-4267-9460-dd232dc7bcd7', pharaonic_exclusive_revelation).
narrative_ontology:cs_drift_state('d401a056-dd20-4267-9460-dd232dc7bcd7', post_amarna_restoration, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d401a056-dd20-4267-9460-dd232dc7bcd7', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_court).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, general_populace).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_theological_exclusivity).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_intermediary_uniqueness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims sole revelatory authority for the pharaoh as the exclusive intermediary of Aten. Dismantles competing temple administrations and redirects their estates to the crown. The pharaoh's personal identity is fused with the Atenist doctrine â name changed, new capital built, artistic program revolutionized. Exit from this framework would require undoing the pharaoh's own legitimation strategy and admitting rival deities.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Previously controlled extensive temple estates and mediated divine legitimacy through Amun-Ra. Under the Atenist reading, their temples are closed, their property seized by the crown, and their ritual role declared illegitimate. They are structurally barred from practicing their traditional function and stripped of economic and political standing, with no alternate institutional channel for legitimacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% Required to redirect all public religious practice through the pharaoh and the Aten. Traditional household and local deity worship is proscribed. While private practice may continue clandestinely, public expression is bounded by state enforcement of the exclusive cult, and the ritual calendar they depend on for social and agricultural coordination is disrupted.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, general_populace, payer,
    powerless, biographical, constrained, national).

% Maintain household and village rituals incorporating traditional deities and syncretic practice. The Atenist reading declares these practices false and illegitimate. Their religious identity is fused with local and familial ritual cycles that are now criminalized, forcing either abandonment or dangerous concealment.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners, payer,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_court).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes all divine legitimacy in the pharaoh, eliminating rival interpretive authorities (priesthoods) and unifying state cult under a single deity and a single human mediator, ostensibly ending factional competition between temples.
% TRANSFER_FUNCTION: Moves temple estates, ritual revenues, and interpretive authority from established priesthoods to the pharaonic court; moves religious compliance and exclusive worship obligation from the populace to the Aten-pharaoh nexus.
% ABSENT_VOICES: The Amun priesthood and folk practitioners are formally excluded from legitimation discourse â their theological claims are declared false rather than debated. Peasant households practicing syncretic religion have no seat at the theological table.
% DISAPPEARANCE_RATIONALE: If the Atenist exclusivity vanished overnight, the Amun priesthood would reclaim temple estates and ritual roles, regional cults would resurface, and the pharaoh would lose the centralized monopoly on divine mediation that the constraint was built to enforce. The political economy of religion in Egypt would revert to distributed temple networks.
% FOUNDING_PROBLEM: The growing wealth and political autonomy of the Theban Amun priesthood threatened pharaonic supremacy; previous reigns saw priestly dynasties rivaling crown power and controlling significant economic resources independent of royal authority.
% FOUNDING_PROBLEM_CORROBORATION: The Amun priesthood attests the crown's fear of their power through their own recorded destruction; later restoration stelae under Tutankhamun and Horemheb corroborate that priestly autonomy had grown threatening to pharaonic authority. Modern Egyptological analysis of pre-Amarna temple estate sizes supports the economic dimension of the rivalry.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at 0.90 (T=8) because the constraint transfers massive temple estates and interpretive authority from the priesthood to the crown while exacting worship compliance from the population. Suppression tracks similarly (0.88 at interval end) because persistence depends on actively closing temples, erasing divine names, and prosecuting alternative practice. Theater_ratio rises from 0.20 to 0.50 as the regime invests heavily in performative infrastructure â new capital, revolutionary art, name changes â that serves both genuine ideological commitment and political theater. Resistance is high (0.85) because the Amun priesthood and regional populations actively resist or subvert the exclusivity claim. Accessibility_collapse is high (0.80) because once the exclusivity doctrine is announced and enforced, legitimate public alternatives nearly vanish, though private practice persists clandestinely.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaonic court seat, the constraint is necessary theological-political hygiene: a dangerously powerful priesthood must be dismantled and legitimacy recentralized to preserve the state. From the priestly and populace seats, the same structure is experienced as confiscation of property, erasure of ancestral identity, and coerced worship of a remote solar disc mediated only by the king. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaonic_court is the concentrated beneficiary and agenda setter, sitting at low directionality (near full beneficiary). Its identity-locked exit and institutional power mean the engine will damp effective extraction for this seat, potentially inverting it into net subsidy. The amun_priesthood, general_populace, and folk_practitioners are the declared victims and payers. The priesthood is trapped (no institutional exit), the populace is constrained, and folk practitioners are identity_locked â all sitting at high directionality, amplifying effective extraction. The divergence between court and subjects should compute as severe seat-level asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination-function lens, one might classify Atenism as a pure snare: it crushes rival priests and seizes wealth. Without the extraction lens, one might classify it as rope or scaffold: it solves a real institutional rivalry. The tangled_rope classification prevents both errors by requiring both a genuine coordination function (centralizing state cult, ending priestly autonomy) and asymmetric extraction (wealth transfer, suppression of alternatives) enforced actively. The temporal measurements show extraction intensifying faster than theater, confirming that the constraint's core dynamic is coercive transfer rather than mere symbolic performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atenist_kernel_reading_location,
    'Does the atenist_monotheistic_reading represent the uniquely extractive member of the divine_legitimacy_substrate kernel, given that its siblings (amun_polytheistic_reading and folk_syncretistic_reading) lack the centralized victim structure?',
    'Comparative structural analysis of the sibling constraints once authored; evaluating whether distributed priestly or household readings generate asymmetric extraction or symmetric coordination.',
    'If siblings show no victims and lower extraction, the Atenist reading is confirmed as the kernel''s tangled_rope member; if siblings also embed extraction, the kernel is uniformly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atenist_kernel_reading_location, conceptual, 'Structural location within contested divine legitimacy kernel').

omega_variable(
    theological_vs_political_driver,
    'Was the Atenist reading primarily driven by genuine theological conviction or by the political-economic imperative to seize temple wealth and dismantle priestly rivals?',
    'Textual analysis of Amarna-period inscriptions for theological consistency versus administrative papyri tracing estate transfers; archaeological assessment of temple closure sequencing relative to theological proclamation.',
    'A predominantly political driver would shift classification toward snare (coordination story as cover); a genuine theological driver sustains tangled_rope (real coordination function alongside extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_driver, empirical, 'Theological sincerity versus political instrumental ambiguity').

omega_variable(
    popular_internalization,
    'Did the general populace and folk practitioners internalize Atenist exclusivity, or did they merely comply structurally while maintaining traditional belief?',
    'Archaeological recovery of non-royal Amarna-period votive objects and post-Amarna restoration testimony; iconographic analysis of private tomb devotion.',
    'If internalized, accessibility_collapse and effective extraction are higher; if merely compliant, theater_ratio and resistance are higher than structural measures suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_internalization, empirical, 'Internalized belief versus structural compliance ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atenist_monotheistic_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(atenist_monotheistic_tr_t2, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(atenist_monotheistic_tr_t4, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(atenist_monotheistic_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(atenist_monotheistic_tr_t8, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(atenist_monotheistic_tr_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(atenist_monotheistic_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(atenist_monotheistic_be_t2, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(atenist_monotheistic_be_t4, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 4, 0.78).
narrative_ontology:measurement(atenist_monotheistic_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.88).
narrative_ontology:measurement(atenist_monotheistic_be_t8, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 8, 0.9).
narrative_ontology:measurement(atenist_monotheistic_be_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 10, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(atenist_monotheistic_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(atenist_monotheistic_su_t2, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 2, 0.72).
narrative_ontology:measurement(atenist_monotheistic_su_t4, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(atenist_monotheistic_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.92).
narrative_ontology:measurement(atenist_monotheistic_su_t8, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 8, 0.9).
narrative_ontology:measurement(atenist_monotheistic_su_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 10, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the divine_legitimacy_substrate kernel. The kernel decomposes into structurally distinct constraints because the label 'divine legitimacy in ancient Egypt' conflates incompatible claims: centralized pharaonic monotheism (this file), distributed priestly polytheism (amun_polytheistic_reading), and household syncretism (folk_syncretistic_reading). Their epsilon values, beneficiary structures, and enforcement mechanisms differ fundamentally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
