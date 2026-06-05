% ============================================================================
% CONSTRAINT STORY: eu_membership_and_exit__sovereignty_lost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_membership_sovereignty_lost, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_membership_and_exit__sovereignty_lost_reading
 *   human_readable: EU Membership Hollowed Parliamentary Sovereignty (Sovereignty Lost Reading)
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   EU membership created a structural subordination of parliamentary
 *   sovereignty that was concealed beneath the doctrine of revocability. The
 *   key doctrinal moment was Factortame (1990-1991), when the ECJ established
 *   that EU law is supreme and prevails over conflicting Acts of Parliament.
 *   This was unprecedented in English law since 1689 — courts had never
 *   before disapplied legislation. The constraint this reading identifies is
 *   not the coordination benefits of EU membership (the sovereignty_pooled
 *   reading emphasizes those) but the extraction of rule-making authority
 *   from democratic electorate and parliament to institutions (ECJ,
 *   Commission, Council) that voters cannot remove through electoral process.
 *   The constraint is snare-class because exit is formally available
 *   (membership is revocable) but the cost of exit is so high that the
 *   revocability is purely theoretical — it conceals subordination in fact.
 *   Over the interval 0-12 (representing 1972-1996, from accession through
 *   Factortame consolidation), extractiveness rose sharply as the legal
 *   doctrines became clear and irreversible, suppression increased as the
 *   finality of the doctrinal choice became apparent, and theater ratio rose
 *   as the gap widened between the ceremonial invocation of parliamentary
 *   sovereignty and the structural reality of subordination.
 *
 * KEY AGENTS:
 *   - Parliamentary Finality: Primary victim (powerless/trapped) — the principle of supreme parliamentary authority has been structurally eliminated by Factortame
 *   - UK Electorate: Secondary victim (moderate/constrained) — cannot remove EU rule-makers through electoral mechanisms; ultimate democratic sanction foreclosed
 *   - Supranational Legal Order (ECJ, Commission, Council): Primary beneficiary (powerful/arbitrage) — achieves binding rule-making authority without continuous democratic sanction in member states
 *   - UK Parliament (institutional actor): (institutional/constrained) — experiences mixed coordination (benefit from trade/regulatory alignment) and extraction (loss of ultimate legislative authority)
 *   - EU Institutional Framework: Institutional beneficiary (institutional/arbitrage) — from its perspective, Factortame doctrine enables coordinated governance across heterogeneous polities
 *   - Sovereignty Doctrine (as institutional inertia): Piton perspective — maintains theatrical invocation of parliamentary supremacy despite structural irrelevance
 *   - Analytical Observer: (analytical/analytical) — risks misclassifying the constraint as natural law (mountain) rather than doctrinal choice (snare)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_membership_and_exit__sovereignty_lost_reading, 0.58).
domain_priors:suppression_score(eu_membership_and_exit__sovereignty_lost_reading, 0.68).
domain_priors:theater_ratio(eu_membership_and_exit__sovereignty_lost_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_lost_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_lost_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eu_membership_and_exit__sovereignty_lost_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_membership_and_exit__sovereignty_lost_reading, snare).
narrative_ontology:human_readable(eu_membership_and_exit__sovereignty_lost_reading, "EU Membership Hollowed Parliamentary Sovereignty (Sovereignty Lost Reading)").
narrative_ontology:topic_domain(eu_membership_and_exit__sovereignty_lost_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(eu_membership_and_exit__sovereignty_lost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_membership_and_exit__sovereignty_lost_reading, 'd252e121-55c0-4892-adf6-fcbc129b13ed').
narrative_ontology:cs_kernel_codification('d252e121-55c0-4892-adf6-fcbc129b13ed', formalized).
narrative_ontology:cs_authority_grounding('d252e121-55c0-4892-adf6-fcbc129b13ed', lineage).
narrative_ontology:cs_interpretation_layer_present('d252e121-55c0-4892-adf6-fcbc129b13ed').
narrative_ontology:cs_reading_relation('d252e121-55c0-4892-adf6-fcbc129b13ed', eu_membership_and_exit__sovereignty_pooled_reading, forecloses).
narrative_ontology:cs_reading_relation('d252e121-55c0-4892-adf6-fcbc129b13ed', eu_membership_and_exit__sovereignty_restored_reading, coexists_with).
narrative_ontology:cs_axiom('d252e121-55c0-4892-adf6-fcbc129b13ed', foundational, revocability_concealed_subordination).
narrative_ontology:cs_axiom_status(revocability_concealed_subordination, holdable).
narrative_ontology:cs_axiom_grounding('d252e121-55c0-4892-adf6-fcbc129b13ed', revocability_concealed_subordination, empirically_contingent).
narrative_ontology:cs_axiom('d252e121-55c0-4892-adf6-fcbc129b13ed', foundational, democratic_accountability_requires_electoral_removal).
narrative_ontology:cs_axiom_status(democratic_accountability_requires_electoral_removal, holdable).
narrative_ontology:cs_axiom_grounding('d252e121-55c0-4892-adf6-fcbc129b13ed', democratic_accountability_requires_electoral_removal, deontological).
narrative_ontology:cs_reference_frame('d252e121-55c0-4892-adf6-fcbc129b13ed', parliamentary_supremacy_doctrine).
narrative_ontology:cs_drift_state('d252e121-55c0-4892-adf6-fcbc129b13ed', factortame_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('d252e121-55c0-4892-adf6-fcbc129b13ed', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(eu_membership_and_exit__sovereignty_lost_reading, eu_membership_and_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_lost_reading, supranational_legal_order).
narrative_ontology:constraint_beneficiary(eu_membership_and_exit__sovereignty_lost_reading, eu_institutional_apparatus).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_lost_reading, parliamentary_finality).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_lost_reading, democratic_accountability).
narrative_ontology:constraint_victim(eu_membership_and_exit__sovereignty_lost_reading, electorate_ultimate_sanction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENTARY FINALITY (SNARE) — Cannot exit the subordination; bears full cost of the Factortame doctrine (courts disapply Acts). The principle of parliamentary supremacy has been structurally eliminated, yet MPs face zero exit option — membership is treated as revocable in theory but subordinating in fact. The victim experiences maximum extraction: loss of legislative authority with no mechanism for recovery short of withdrawal.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UK ELECTORATE (SNARE) — Constrained by the fact that they cannot remove EU rule-makers (European Commission, Court of Justice) via electoral mechanisms. Ultimate democratic sanction — voting out those who make binding rules — is foreclosed for rule-making now exported to Brussels. Extraction is severe: voters pay the cost of rules they cannot revoke through democratic process. The 'revocable in theory' framing conceals that revocation requires collective withdrawal, not electoral remedy.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPRANATIONAL LEGAL ORDER (SNARE) — Achieves rule-making authority that cannot be revoked by any single member state without collective agreement or full withdrawal. The beneficiary experiences this as coordination (achieving binding commitments across heterogeneous polities) but the structure is extraction: binding authority over rule-making established without requiring continuous democratic sanction in each polity. Arbitrage access allows selective withdrawal from specific EU competencies without full withdrawal — unavailable to those trapped in the system.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UK PARLIAMENT (TANGLED ROPE) — Parliament experiences mixed coordination and extraction. It has genuine coordination function: EU membership solved coordination problems (trade frictionlessness, regulatory alignment, cross-border worker mobility). But Parliament's supreme legislative authority is the cost. Parliament's continued presence in the system is constrained by the political reality that withdrawal requires parliamentary consent but the legal structure makes continuous re-approval of EU supremacy structurally difficult to refuse (exit is politically costly even when formally available).
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU INSTITUTIONAL FRAMEWORK (ROPE) — From its own perspective, the supremacy doctrine is pure coordination: it solves the collective action problem of ensuring that EU rules are not undermined by conflicting member-state legislation. The framework experiences the extraction as functional necessity, not coercion. Arbitrage access (ability to negotiate opt-outs, special status, selective non-participation in some competencies) allows institutional flexibility unavailable to member-state actors. The rope classification reflects that EU institutions see Factortame as enabling cooperation, not suppressing sovereignty.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: SOVEREIGNTY DOCTRINE AS PITON (INSTITUTIONAL INERTIA) — The doctrine of parliamentary supremacy persists in constitutional mythology long after its structural function has been hollowed. Courts invoke 'sovereignty remains supreme' while simultaneously disapplying sovereign legislation (Factortame). This is pure theater: the invocation reassures citizens that their parliament retains ultimate authority while the legal system operates on the basis that it does not. Theater ratio high because the doctrine's ceremonial reassurance persists despite structural irrelevance. The piton persists through inertia (teaching it, invoking it, maintaining the pretense) rather than through any current functional force.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational perspective, one might argue that supranational legal order is inherent to any multi-state system seeking binding commitments — that supremacy of coordinating law is a natural feature of federal-like arrangements. This would classify the constraint as a structural inevitability (Mountain). However, the beneficiary presence (supranational_legal_order) and the measurable suppression of democratic accountability signal that this is a false summit: what appears as natural necessity is actually a particular legal doctrinal choice (Factortame) that benefits specific institutions. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_membership_and_exit__sovereignty_lost_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_membership_and_exit__sovereignty_lost_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_membership_and_exit__sovereignty_lost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_membership_and_exit__sovereignty_lost_reading, TR),
    TR >= 0.70.

:- end_tests(eu_membership_and_exit__sovereignty_lost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Measured as the proportion of rule-making authority exported from democratic electoral control. The measurement trajectory (0.15→0.42→0.58) reflects the crystallization of Factortame doctrine and its integration into the constitutional order. At t0 (1972), the impact was not yet clear; the European Communities Act was framed as preserving parliamentary sovereignty through a revocable delegation. At t6 (1984-1986, as the ECJ's expansionism became clear in Costa, Van Gend, Cassis de Dijon), extractiveness was moderate — the scope of supremacy was becoming evident. At t12 (1994-1996, post-Factortame), extractiveness was severe — courts were openly disapplying Acts, the irreversibility of the structure was apparent, and the fiction of revocability was exposed. Suppression (0.68): Measured as the combined effect of (a) absence of electoral exit for EU rule-makers, (b) legal doctrine making parliamentary override of EU law impossible without withdrawal, and (c) political/economic costs of withdrawal that make formal revocability irrelevant. The measurement trajectory (0.52→0.61→0.68) reflects increasing awareness that formal options (revocation) are structurally suppressed by political cost. Theater ratio (0.62): Measured as the gap between ceremonial invocation of parliamentary sovereignty and structural subordination. The trajectory (0.48→0.55→0.62) reflects the widening gap as Factortame made clear what the legal system actually does (disapplies Acts) while constitutional discourse continued to invoke parliamentary supremacy as if it remained operative.
 *
 * PERSPECTIVAL GAP:
 *   This reading's core claim is that membership subordinates parliamentary sovereignty in fact despite being revocable in theory. The snare classification at the powerless/trapped perspective reflects the victim's actual structural position: they cannot exit without prohibitive cost. The snare classification from the moderate/constrained electorate perspective reflects that democratic accountability is structurally foreclosed — even if the electorate votes to change course, the rule-making authority (ECJ, Commission) remains unaccountable to that electorate. The piton perspective on the sovereignty doctrine reflects that parliamentary supremacy persists as ceremonial reassurance despite structural irrelevance. The beneficiary perspectives (institutional actors, analytical observers) risk naturalizing what is a contingent doctrinal choice — they may classify the constraint as mountain (natural feature of federal systems) rather than snare (extraction benefit to those wielding supranational authority). The sovereignty_pooled reading (the sibling) would emphasize the coordination benefits and frame membership as a continuing, revocable choice. This reading forecloses that frame by insisting that revocability is illusory — the choice, once made, cannot be unmade at acceptable cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to rule-making authority export. Parliamentary finality (victim, trapped) receives d≈0.95 — maximum target position, no exit. UK electorate (victim, constrained) receives d≈0.88 — high target position, constrained exit through electoral mechanisms that cannot reach EU institutions. Supranational legal order (beneficiary, arbitrage) receives d≈0.15 — low target position, benefits from the extraction, has exit-option flexibility unavailable to trapped agents. EU institutional framework (beneficiary, arbitrage) receives d≈0.12 — even lower, because from its perspective the structure is functional coordination. The f(d) sigmoid converts these d values to experienced extractiveness multipliers: trapped agents experience χ severely amplified; beneficiaries experience χ dampened or negative (they benefit from the structure). Scope (global for beneficiary, national for victim) applies σ(S) scaling: beneficiary's global scope (σ=1.2) amplifies their positive extraction; victim's national scope (σ=0.8) dampens their negative experience, but the base is so severe (d≈0.95, suppression=0.68) that dampening is minimal.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly resolves mandatrophy by adopting the snare classification — pure extraction (ε≈0.58, χ≈0.66 at the powerless perspective, accounting for scope dampening). The mandatrophy is resolved by rejecting the reframing-as-mountain: this constraint is NOT a natural law of federal systems (that would be the false summit). It IS a particular doctrinal choice (Factortame) that benefits specific institutional actors (ECJ, Commission, Council) at the cost of democratic accountability. The snare classification reflects the structural reality: beneficiaries can exit selectively (arbitrage); victims cannot exit at acceptable cost (suppression). The theater (0.62) reflects the gap between what the legal system actually does and what constitutional discourse claims it does — a gap that stabilizes the snare by keeping victims convinced their ultimate sanction remains when in fact it does not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revocability_concealment,
    'Is EU membership genuinely revocable in practice, or does the ''revocable in theory'' framing conceal de facto irreversibility?',
    'Historical observation: the 2016 referendum and Article 50 invocation demonstrated formal revocability. But assessment requires examining the political, economic, and constitutional costs of invocation — if costs are prohibitively high, ''revocable in theory'' conceals subordination in fact.',
    'If genuinely revocable with acceptable cost: constraint reclassifies toward Tangled Rope (mixed coordination and extraction, but not pure snare). If costs prohibitive: snare classification confirmed (extraction locked by suppression of exit path).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revocability_concealment, empirical, 'Whether EU membership revocability is de facto or merely de jure').

omega_variable(
    factortame_doctrine_necessity,
    'Was the Factortame doctrine (primacy of EU law, disapplying conflicting Acts) a necessary consequence of EU membership, or a doctrinal choice that could have been avoided?',
    'Comparative constitutional analysis: examining how other multi-state systems (Swiss cantons, Australian federation, US federal system) achieve binding commitments without requiring supremacy of federal law over state law to reach the Zeppel level of EU supremacy. Analysis of alternative doctrines (EU law as binding but not supreme; parallel hierarchies; mutual recognition without hierarchy).',
    'If necessary: the constraint is a mountain (structural feature of federal coordination). If contingent doctrinal choice: constraint is snare (extraction benefit accrues to those wielding EU law authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factortame_doctrine_necessity, conceptual, 'Whether Factortame doctrine was inevitable or contingent').

omega_variable(
    parliamentary_consent_fiction,
    'Does parliamentary consent to EU membership genuinely constitute an informed, revocable delegation of authority, or does it conceal permanent subordination beneath the fiction of ongoing choice?',
    'Analysis of parliamentary debate and voting records at key moments (1972 European Communities Act, Maastricht ratification, Lisbon ratification, 2015 referendum legislation). Examination of whether subsequent parliaments could realistically reverse prior parliamentary consent, and whether voters understood membership as revocable or as permanent structural subordination.',
    'If genuine ongoing choice: constraint is Tangled Rope with significant coordination benefits (supranational problem-solving). If fiction: constraint is pure Snare (extraction locked by suppression of meaningful exit choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_consent_fiction, empirical, 'Whether parliamentary consent to EU membership was informed and revocable').

omega_variable(
    reading_discrimination,
    'Does this reading (sovereignty_lost) genuinely foreclose the sovereignty_pooled reading, or do they coexist as irreconcilable but simultaneously held frames?',
    'Analysis of how different legal and political actors deploy these readings: Do advocates of parliamentary sovereignty explicitly deny that coordination benefits exist (foreclosure)? Or do they acknowledge pooling while insisting finality was never surrendered (coexistence)? Do the readings occupy different institutional positions (courts vs Parliament vs voters)?',
    'If foreclosure: the sovereignty_lost reading is an absolute truth claim; sibling readings are logically impossible. If coexistence: both readings capture real structural features; the constraint manifests differently depending on observer position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_discrimination, conceptual, 'Whether sovereignty_lost forecloses or coexists with sovereignty_pooled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_membership_and_exit__sovereignty_lost_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_sovlost_theater_t0, eu_membership_and_exit__sovereignty_lost_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(eu_sovlost_theater_t6, eu_membership_and_exit__sovereignty_lost_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(eu_sovlost_theater_t12, eu_membership_and_exit__sovereignty_lost_reading, theater_ratio, 12, 0.62).

% Extraction over time
narrative_ontology:measurement(eu_sovlost_extract_t0, eu_membership_and_exit__sovereignty_lost_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(eu_sovlost_extract_t6, eu_membership_and_exit__sovereignty_lost_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(eu_sovlost_extract_t12, eu_membership_and_exit__sovereignty_lost_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eu_sovlost_suppress_t0, eu_membership_and_exit__sovereignty_lost_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(eu_sovlost_suppress_t6, eu_membership_and_exit__sovereignty_lost_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(eu_sovlost_suppress_t12, eu_membership_and_exit__sovereignty_lost_reading, suppression_requirement, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_membership_and_exit__sovereignty_lost_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_membership_and_exit__sovereignty_lost_reading, eu_membership_and_exit__sovereignty_pooled_reading).
narrative_ontology:affects_constraint(eu_membership_and_exit__sovereignty_lost_reading, eu_membership_and_exit__sovereignty_restored_reading).

% DUAL FORMULATION NOTE:
% The kernel 'EU membership and exit' has three structurally distinct readings instantiated as three separate constraint stories. The sovereignty_lost_reading (this file) models the constraint as snare-class extraction. The sovereignty_pooled_reading models the same doctrinal arrangements as rope-class coordination. The sovereignty_restored_reading models the institutional capacity to reverse the arrangements. These are NOT measurements of the same constraint from different observables — they are three distinct constraints from the same kernel, each with different beneficiaries, victims, and extractiveness profiles. The three stories form a constraint family linked by affects_constraints edges. Each reading instantiates ONE coherent interpretation of the contested kernel; no single story attempts to hold multiple readings or hedge between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_membership_and_exit__sovereignty_lost_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
