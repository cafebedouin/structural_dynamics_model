% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto Protecting Particularist Privileges
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   The right of remonstrance (droit de remontrance) was the power of French
 *   sovereign courts (parlements) to protest royal edicts before registering
 *   them. Under the crown_reading, this evolved from a procedural advisory
 *   function into a substantive minoritarian veto that protected the
 *   particularist privileges of magistrates (venal office property, judicial
 *   autonomy), the nobility and clergy (tax exemptions), and provincial
 *   estates (fiscal particularism) against royal fiscal reform. The Crown
 *   experienced this as high extraction on its fiscal authority: blocked
 *   revenue edicts forced borrowing at penalty rates, delayed military
 *   funding, and prevented structural tax reform. The magistrates'
 *   constitutional claim was suppressed as illegitimate obstruction by
 *   successive royal ministries, culminating in the Maupeou coup of 1771
 *   which abolished the parlements. This constraint story captures the
 *   Crown's structural experience of the remonstrance as a snare: a
 *   coordination cover (legislative review) masking asymmetric extraction
 *   (privilege protection) maintained by active enforcement (magistrate
 *   resistance, public opinion manipulation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.82).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.75).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto Protecting Particularist Privileges").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '9eb59173-4ccc-4ad6-b36d-c24de195cfa0').
narrative_ontology:cs_kernel_codification('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', formalized).
narrative_ontology:cs_authority_grounding('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', lineage).
narrative_ontology:cs_interpretation_layer_present('9eb59173-4ccc-4ad6-b36d-c24de195cfa0').
narrative_ontology:cs_reading_relation('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', foundational, remonstrance_is_usurpation_not_right).
narrative_ontology:cs_axiom_status(remonstrance_is_usurpation_not_right, holdable).
narrative_ontology:cs_axiom_grounding('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', remonstrance_is_usurpation_not_right, conventional).
narrative_ontology:cs_axiom('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', foundational, royal_fiscal_authority_is_sovereign).
narrative_ontology:cs_axiom_status(royal_fiscal_authority_is_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', royal_fiscal_authority_is_sovereign, conventional).
narrative_ontology:cs_reference_frame('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', ancien_regime_royal_sovereignty).
narrative_ontology:cs_drift_state('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', pre_revolutionary_crisis, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('9eb59173-4ccc-4ad6-b36d-c24de195cfa0', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, parlement_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_estates_privileged).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, tax_farmers_general).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, royal_prerogative_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, sovereign_legislative_monopoly).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, unity_of_sovereign_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues royal edicts requiring registration by the parlements; bears fiscal and legislative costs when remonstrance blocks or delays revenue edicts; can force registration via lit de justice but at high political cost and legitimacy erosion.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, crown, payer).

% Hold venal offices granting right of remonstrance; use veto to protect judicial autonomy, venal office value, and particularist privileges; frame obstruction as constitutional duty; their professional identity and social status are fused with the remonstrance right.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parlement_magistrates, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, parlement_magistrates, agenda_setter).

% Purchased judicial offices whose value depends on the parlement's veto power; any royal reform threatening remonstrance devalues their property; they finance and socially reinforce magistrate resistance.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_office_holders, beneficiary,
    organized, biographical, identity_locked, national).

% Provincial representative bodies with negotiated tax privileges; rely on parlement remonstrance to block uniform fiscal edicts that would extend taxation to their exempt orders; coordinate with magistrates to preserve fiscal particularism.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_estates_privileged, beneficiary,
    organized, biographical, constrained, regional).

% Bears direct financial cost of delayed or blocked revenue edicts; incurs borrowing at penalty rates when remonstrance stalls fiscal reform; cannot exit the need for revenue but cannot compel registration without political crisis.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_treasury, payer,
    institutional, biographical, constrained, national).

% Finance royal revenue in advance; their contracts lose value when remonstrance disrupts tax collection; can exert market pressure but are structurally dependent on royal fiscal authority that the veto undermines.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, tax_farmers_general, payer,
    powerful, biographical, mobile, national).

% Bears the ultimate tax burden when noblesse and clergy are exempted by particularist privileges the remonstrance protects; has no voice in remonstrance proceedings and no exit from the fiscal system.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, third_estate, excluded,
    powerless, immediate, trapped, local).

% Analyze the remonstrance as either a constitutional safeguard (magistrate reading) or an illegitimate veto (crown reading); their framing shapes later historical and legal interpretation but they hold no operational power in the period.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The remonstrance purports to coordinate legislative review by allowing courts to advise the king on legality before registration, preventing arbitrary innovation. From the crown reading, this coordination story is cover for a veto that protects magistrate and privilege-holder rents.
% TRANSFER_FUNCTION: Moves effective legislative initiative and fiscal authority from the Crown to the parlements; moves fiscal rents from the royal treasury and tax-farming system to venal office holders and privileged orders via the veto's protection of exemptions; moves political legitimacy from sovereign will to corporate magistrate authority.
% ABSENT_VOICES: The third estate (peasantry, urban workers, bourgeoisie outside venal office) who bear the tax burden of the veto's protection of noble and clerical exemptions; provincial populations subject to unequal taxation; future taxpayers who inherit debt from blocked reforms. They are structurally excluded from the remonstrance proceeding and the parlements' composition.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, royal edicts would register without delay; fiscal reforms extending taxation to privileged orders would proceed; venal office values would collapse; the parlements would lose their political veto and revert to pure judicial function; the fiscal-military state would centralize revenue extraction.
% FOUNDING_PROBLEM: The remonstrance originated in the 14th-15th centuries as a mechanism for sovereign courts to advise the king on the legality and form of edicts before registration, ensuring technical conformity with fundamental laws. It evolved through the Wars of Religion and Fronde into a substantive veto right claimed as constitutional necessity.
% FOUNDING_PROBLEM_CORROBORATION: Royal administrators (intendants, controllers-general) consistently attested from the 1660s onward that the veto blocked necessary fiscal reform; the magistrates' own registers show the evolution from procedural verification to substantive policy veto; Enlightenment jurists outside the magistrate corps (e.g., Linguet, though briefly a magistrate, wrote against the veto; the Physiocrats condemned it as anti-economic) corroborated the shifted-function reading. The Maupeou reform of 1771, which suppressed the parlements and replaced them with royal courts, demonstrates the Crown's own judgment that the founding problem was dead.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the veto directly transferred fiscal authority from Crown to magistrates, forcing the treasury into costly debt and blocking reforms that would have spread taxation more broadly. Suppression is high (0.75) because the Crown had to deploy lit de justice, exile magistrates, or abolish parlements to overcome the veto — active coercion against a coordinate branch. Theater ratio is moderate (0.48): the remonstrance ceremony and written remonstrances performed constitutional legitimacy, but an increasing share of activity was rent-protection. Accessibility collapse is high (0.78): no alternative path for royal edicts existed; the parlements were the mandatory registration venue. Resistance is substantial (0.62): magistrates used strike, exile, public propaganda, and aristocratic alliance to defend the veto.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the Crown's seat, high ε + high suppression + constrained exit = snare; from the magistrate seat, low ε (they collect) + identity_locked exit = rope (they see genuine coordination); from the third estate seat, excluded + trapped = snare victim. The crown_reading's claimed_type (snare) reflects the Crown's structural experience; the magistrate_reading would claim rope or mountain. This divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown is the agenda_setter (issues edicts) but becomes payer when thwarted — its directionality is pulled toward target (d ~0.7) because the constraint extracts fiscal authority from it. Magistrates are beneficiaries (collect veto rents) but also agenda_setters (initiate remonstrances) — their identity_locked exit (offices are venal, identity fused with remonstrance) keeps d low (~0.2). Venal office holders and provincial estates are beneficiaries with identity_locked/constrained exit. The royal treasury and tax farmers are payers with constrained/mobile exit. The third estate is excluded and trapped. Constitutional theorists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (procedural legality review) is dead — the veto evolved into a substantive policy block protecting particularist fiscal privileges. The arrangement persists because magistrates' venal property rights and noble/clerical tax exemptions depend on it. No beneficiary has incentive to reform; the Crown could fix it only at prohibitive political cost (Maupeou showed the cost). This is classic mandatrophy: the constraint's mandate (review for legality) has atrophied, but the structure persists as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the remonstrance authority a single constraint with observer-dependent classification, or two distinct constraints (crown_reading and magistrate_reading) with different ε referents?',
    'Apply ε-invariance test: if measuring the remonstrance from Crown''s structural position yields ε=0.82 and from magistrate''s position yields ε=0.15, these are two constraints, not one. The kernel decomposition into separate constraint stories with linked network.affects_constraints is the correct modeling choice.',
    'If single constraint, classification is unstable and observer-relative. If two constraints, each gets stable ε and the engine can model their structural conflict via network edges. The current JSON models crown_reading only; magistrate_reading must be a separate file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the remonstrance kernel decomposes into multiple ε-invariant constraints.').

omega_variable(
    remonstrance_natural_law_vs_constructed_privilege,
    'Is the remonstrance right a genuine fundamental law of the French monarchy (mountain) or a constructed privilege that evolved into extraction (snare)?',
    'Historical analysis of the remonstrance''s origin: if it was a royal concession that became claimed as immemorial right, it is constructed. The Maupeou abolition (1771) and restoration (1774) demonstrate its contingent, political nature. The 1788 ''droit de remontrance'' restoration by Brienne confirms it as a political bargaining chip, not natural law.',
    'If constructed, the crown_reading''s snare classification holds; if fundamental law, the magistrate_reading''s mountain/rope classification holds for their seat. The engine''s false_summit_mountain signature would trigger if magistrate_reading claimed mountain with declared beneficiaries (magistrates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remonstrance_natural_law_vs_constructed_privilege, empirical, 'Whether the remonstrance''s constitutional status is natural/fixed or political/constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of magistrate legitimacy (by Crown) structural (royal coercion: lit de justice, exile, abolition) or internalized (magistrates'' own constitutional scruples limiting their veto)?',
    'Track magistrate behavior when suppression lifts: after Maupeou abolition (1771), magistrates did not abandon remonstrance claim; after 1774 restoration, they immediately resumed veto. This suggests internalized identity-lock, not merely structural coercion. The suppression metric (0.75) captures structural force; internalized dimension would require higher effective suppression.',
    'If internalized, the magistrate seat''s effective extraction is lower than structural metrics suggest (they self-limit); if purely structural, the Crown''s suppression must be continuously maintained. Affects whether the constraint is snare (requires active enforcement) or piton (atrophied but identity-maintained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for magistrate identity-locked agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remonstrance_crown_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(remonstrance_crown_tr_t20, remonstrance_authority__crown_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(remonstrance_crown_tr_t40, remonstrance_authority__crown_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(remonstrance_crown_tr_t60, remonstrance_authority__crown_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(remonstrance_crown_tr_t80, remonstrance_authority__crown_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(remonstrance_crown_tr_t100, remonstrance_authority__crown_reading, theater_ratio, 100, 0.46).
narrative_ontology:measurement(remonstrance_crown_tr_t120, remonstrance_authority__crown_reading, theater_ratio, 120, 0.47).
narrative_ontology:measurement(remonstrance_crown_tr_t140, remonstrance_authority__crown_reading, theater_ratio, 140, 0.48).

% Extraction over time
narrative_ontology:measurement(remonstrance_crown_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(remonstrance_crown_be_t20, remonstrance_authority__crown_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(remonstrance_crown_be_t40, remonstrance_authority__crown_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(remonstrance_crown_be_t60, remonstrance_authority__crown_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(remonstrance_crown_be_t80, remonstrance_authority__crown_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(remonstrance_crown_be_t100, remonstrance_authority__crown_reading, base_extractiveness, 100, 0.77).
narrative_ontology:measurement(remonstrance_crown_be_t120, remonstrance_authority__crown_reading, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(remonstrance_crown_be_t140, remonstrance_authority__crown_reading, base_extractiveness, 140, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(remonstrance_crown_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(remonstrance_crown_su_t20, remonstrance_authority__crown_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(remonstrance_crown_su_t40, remonstrance_authority__crown_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(remonstrance_crown_su_t60, remonstrance_authority__crown_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(remonstrance_crown_su_t80, remonstrance_authority__crown_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(remonstrance_crown_su_t100, remonstrance_authority__crown_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement(remonstrance_crown_su_t120, remonstrance_authority__crown_reading, suppression_requirement, 120, 0.74).
narrative_ontology:measurement(remonstrance_crown_su_t140, remonstrance_authority__crown_reading, suppression_requirement, 140, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__crown_reading, 0.12).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the remonstrance_authority kernel into crown_reading (veto as illegitimate extraction: high ε for Crown, magistrates as beneficiaries) and magistrate_reading (veto as constitutional safeguard: low ε, Crown as extractor). The crown_reading assigns ε=0.82 to royal fiscal authority thwarted; the magistrate_reading would assign ε≈0.15 to arbitrary royal innovation unchecked. They are linked via network.affects_constraints because the Crown's suppression of magistrate legitimacy structurally depends on the magistrate_reading's claim being falsified, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
