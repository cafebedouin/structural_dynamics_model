% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of Treaty of Waitangi Article I
 *   domain: constitutional/political/indigenous_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the crown_sovereignty_reading of the
 *   waitangi_sovereignty_allocation kernel. Under this reading, the English
 *   text of the Treaty of Waitangi Article I ceded complete sovereignty to
 *   the British Crown, establishing Westminster parliamentary supremacy over
 *   New Zealand. The constraint allocates plenary legislative and
 *   resource-allocation power to the Crown/Parliament without requiring
 *   MÄori consent, subordinating iwi and hapÅ« interests to parliamentary
 *   will. It is actively enforced through the New Zealand judiciary,
 *   legislative processes, and executive action. The coordination
 *   functionâunified colonial and later national governanceâis real, but
 *   asymmetric extraction of sovereignty and resources from MÄori is equally
 *   real. The claim is tangled_rope: a genuine governance-coordination
 *   mechanism that simultaneously operates as a structure of unilateral
 *   extraction.
 *
 * KEY AGENTS:
 *   - crown_parliament: Primary beneficiary (institutional/arbitrage) â collects sovereignty and unilateral legislative power
 *   - maori_iwi_hapu: Primary target (organized/identity_locked) â bears extraction through subordinated sovereignty and loss of jurisdictional authority
 *   - nz_judiciary: Secondary actor (institutional/constrained) â enforces the reading through constitutional interpretation and precedent
 *   - settler_majority: Secondary beneficiary (powerful/mobile) â benefits from parliamentary supremacy and unilateral resource allocation
 *   - british_crown: Historical beneficiary (institutional/arbitrage) â originally received the ceded sovereignty under the English text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.75).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of Treaty of Waitangi Article I").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/political/indigenous_rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'b3b094dd-bbc5-4683-8834-ce76aadac77f').
narrative_ontology:cs_kernel_codification('b3b094dd-bbc5-4683-8834-ce76aadac77f', fixed_text).
narrative_ontology:cs_authority_grounding('b3b094dd-bbc5-4683-8834-ce76aadac77f', lineage).
narrative_ontology:cs_interpretation_layer_present('b3b094dd-bbc5-4683-8834-ce76aadac77f').
narrative_ontology:cs_reading_relation('b3b094dd-bbc5-4683-8834-ce76aadac77f', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_reading_relation('b3b094dd-bbc5-4683-8834-ce76aadac77f', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('b3b094dd-bbc5-4683-8834-ce76aadac77f', foundational, parliamentary_supremacy_absolute).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b3b094dd-bbc5-4683-8834-ce76aadac77f', parliamentary_supremacy_absolute, conventional).
narrative_ontology:cs_axiom('b3b094dd-bbc5-4683-8834-ce76aadac77f', foundational, complete_sovereignty_ceded).
narrative_ontology:cs_axiom_status(complete_sovereignty_ceded, holdable).
narrative_ontology:cs_axiom_grounding('b3b094dd-bbc5-4683-8834-ce76aadac77f', complete_sovereignty_ceded, empirically_contingent).
narrative_ontology:cs_reference_frame('b3b094dd-bbc5-4683-8834-ce76aadac77f', westminster_supremacy_framework).
narrative_ontology:cs_drift_state('b3b094dd-bbc5-4683-8834-ce76aadac77f', post_waitangi_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3b094dd-bbc5-4683-8834-ce76aadac77f', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_majority).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_crown).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises plenary legislative power over New Zealand without requiring MÄori consent. Sets and enforces the constitutional framework that allocates sovereignty to the Crown and subordinates MÄori governance to parliamentary will. Collects the extraction of sovereign authority and unilateral resource-allocation power.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament, beneficiary).

% Collective groups whose tino rangatiratanga over lands, waters, and taonga is subordinated to parliamentary will under this reading. They retain cultural and political organization but lack veto or consent power over legislation affecting their interests and territories. Exit is identity-locked because their sovereign identity is constituted through relationship to whenua and taonga that the constraint governs.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, national).

% Benefits from a unified governance structure and parliamentary supremacy that facilitates economic development, property tenure, and resource allocation without indigenous consent requirements. Their political and economic lives operate within the framework as unmarked citizens.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_majority, beneficiary,
    powerful, biographical, mobile, national).

% Interprets and upholds the doctrine of parliamentary supremacy and Crown sovereignty as received from English constitutional law. Validates legislation and executive action without requiring MÄori consent, bound by precedent and the constitutional hierarchy this reading establishes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Original recipient of the ceded sovereignty under the English Article I framing. Transferred operational authority to New Zealand institutions while retaining symbolic and constitutional status. Benefits from the global continuity of the imperial legal tradition.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_crown, beneficiary,
    institutional, civilizational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified sovereign authority and Westminster-style parliamentary governance over New Zealand territory, coordinating law-making, resource allocation, and enforcement under a single hierarchical chain of command.
% TRANSFER_FUNCTION: Transfers plenary sovereignty, legislative supremacy, and unilateral resource-allocation power from MÄori to the Crown/Parliament; transfers the obligation of compliance from MÄori to parliamentary will and the common law.
% ABSENT_VOICES: MÄori signatories and rangatira who understood the MÄori text to retain tino rangatiratanga; contemporary MÄori jurists and tikanga authorities who read the Treaty as preserving MÄori governance or as creating an ongoing partnership. They are present in public discourse but structurally excluded from the sovereignty-allocation mechanism.
% DISAPPEARANCE_RATIONALE: If the Crown sovereignty reading vanished overnight, parliamentary supremacy would lose its foundational claim to exclusive authority; MÄori governance structures would reassert jurisdictional presence; the New Zealand constitutional order would require fundamental renegotiation around co-sovereignty or partnership.
% FOUNDING_PROBLEM: British colonial need for unified governance over New Zealand territory and subjects; prevention of settler lawlessness; resolution of competing sovereignty claims among MÄori tribes and European powers.
% FOUNDING_PROBLEM_CORROBORATION: Colonial Office records from 1839-1840 attest the security motive from the British imperial side. MÄori historians and the Waitangi Tribunal attest the problem was constructed to serve imperial acquisition rather than local governance needs. Academic constitutional historians outside both direct beneficiary and victim seats corroborate the colonial security motive while contesting its legitimacy and necessity.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers sovereignty, jurisdiction, and resource-allocation power unilaterally from MÄori to the Crown. Suppression (0.75) is high because persistence requires active judicial enforcement, legislative override of MÄori objections, and suppression of competing jurisdictional claims. Theater_ratio (0.65 at interval end) is substantial: the modern Treaty settlement process and 'Crown honor' discourse perform reconciliation while leaving the underlying sovereignty allocation untouched. Accessibility_collapse (0.70) is high because, within the Westminster framework, MÄori sovereignty alternatives appear legally impossible. Resistance (0.60) reflects ongoing Treaty claims, protests, and jurisdictional assertions.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown/Parliament seat, the constraint appears as necessary constitutional orderâthe only alternative presented is anarchy or fragmented sovereignty. From the MÄori seat, the same constraint appears as the active suppression of tino rangatiratanga through an alien legal framework. The engine computes this divergence: the agenda_setter and beneficiary seats should classify as rope-like or low-extraction, while the payer seat should classify as snare-like or high-extraction. The authored claim of tangled_rope captures the structural fusion of these two realities.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown/Parliament and the settler majority are structural beneficiaries: they receive low directionality (d near the beneficiary end) because the constraint subsidizes their legislative and resource power. The NZ judiciary sits near symmetric but slightly toward beneficiary: it is structurally bound to uphold the framework but does not personally collect. MÄori iwi and hapÅ« are structural targets: high directionality (d near the target end) because the constraint extracts sovereignty and jurisdiction from them; their exit is identity_locked, amplifying effective extraction because they cannot abandon their relationship to whenua and taonga without existential cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâestablishing orderly colonial governance and preventing inter-tribal/settler violenceâwas substantially solved by the mid-20th century, yet the constraint persists in its original form rather than transitioning to a post-colonial power-sharing framework. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals mandatrophy: the arrangement has outlived its colonial mandate but continues because the Crown and settler majority benefit from the sovereignty allocation. The Tangled Rope classification prevents mislabeling this as pure coordination (Rope) by requiring named victims and active enforcement, and prevents mislabeling it as pure extraction (Snare) by acknowledging the real governance-coordination function it provides to the settler state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_multi_stability,
    'Is the waitangi_sovereignty_allocation kernel permanently multi-stable due to bilingual textual divergence, or can one reading be shown as the single correct interpretation?',
    'Comparative legal-hermeneutic analysis across all three readings; assessment of whether any single reading can accommodate both the English and MÄori texts without contradiction.',
    'If the kernel is irreducibly contested, the Crown sovereignty reading''s enforcement against MÄori governance claims is revealed as structural suppression of legitimate alternatives, not neutral interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_multi_stability, conceptual, 'Whether the Treaty sovereignty kernel is permanently multi-stable').

omega_variable(
    maori_intent_1840,
    'Did MÄori signatories in 1840 understand and intend to cede complete sovereignty to the Crown, or did they understand kÄwanatanga as limited governorship?',
    'Historical-linguistic analysis of te reo MÄori usage in 1840, combined with ethnohistorical records of pre-signing debate and post-signing resistance.',
    'If MÄori did not intend to cede complete sovereignty, the empirical foundation of this reading collapses, shifting classification toward snare (coordination story as cover for pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maori_intent_1840, empirical, 'Empirical uncertainty about the intent behind Article I').

omega_variable(
    settlement_theater_extraction,
    'Does the modern Treaty settlement process reduce extraction from MÄori, or does it theatricalize justice while preserving the underlying sovereignty allocation?',
    'Longitudinal analysis of settlement outcomes versus initial claims; measurement of whether settled iwi recover jurisdictional authority or only commercial redress within Crown law.',
    'If settlements are primarily theatrical, theater_ratio is higher than baseline suggests and extraction persists through performative reconciliation; if substantive, extractiveness may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_theater_extraction, empirical, 'Whether modern settlements are substantive or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wait_tr_t36, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(wait_tr_t72, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 72, 0.5).
narrative_ontology:measurement(wait_tr_t108, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 108, 0.55).
narrative_ontology:measurement(wait_tr_t144, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 144, 0.6).
narrative_ontology:measurement(wait_tr_t180, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 180, 0.65).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(wait_be_t36, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement(wait_be_t72, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 72, 0.78).
narrative_ontology:measurement(wait_be_t108, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 108, 0.75).
narrative_ontology:measurement(wait_be_t144, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 144, 0.76).
narrative_ontology:measurement(wait_be_t180, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 180, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(wait_su_t36, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 36, 0.85).
narrative_ontology:measurement(wait_su_t72, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 72, 0.8).
narrative_ontology:measurement(wait_su_t108, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 108, 0.7).
narrative_ontology:measurement(wait_su_t144, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 144, 0.65).
narrative_ontology:measurement(wait_su_t180, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 180, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the waitangi_sovereignty_allocation kernel, which decomposes into three structurally distinct claims: Crown sovereignty (this story), ongoing partnership, and retained rangatiratanga. Each has different epsilon values, beneficiaries, and victim sets. Linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
