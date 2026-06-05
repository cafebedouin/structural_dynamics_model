% ============================================================================
% CONSTRAINT STORY: french_presidential_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_presidential_authority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: french_presidential_authority
 *   human_readable: French Presidential Authority Under Cohabitation
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   French presidential authority under the Fifth Republic exhibits a
 *   structural constraint that oscillates between unified and divided
 *   government regimes. During periods of unified government (president and
 *   parliament from same coalition), the presidential office experiences its
 *   authority as pure coordination: constitutional delegation of foreign
 *   policy, defense, and treaty authority to the president aligns with
 *   parliamentary will, creating no extraction asymmetry. During cohabitation
 *   (1986-1988, 1993-1995, 1997-2002), the same constitutional framework
 *   produces asymmetric extraction: the president retains veto power over
 *   treaties and defense, constraining the parliamentary majority's
 *   agenda-setting authority, while the prime minister controls domestic
 *   policy, constraining the president's domestic reach. This creates a
 *   tangled rope structure: genuine coordination function (preventing
 *   parliamentary fragmentation on security matters and maintaining state
 *   coherence in foreign policy) alongside extraction (suppression of
 *   legislative autonomy in high-stakes domains). The constraint's theater
 *   ratio increased sharply during cohabitation (1986) as both offices
 *   performed executive authority without clear power separation, creating
 *   elaborate ceremonial roles to manage jurisdictional ambiguity.
 *   Post-cohabitation periods show theater decreasing as unified government
 *   clarifies roles, but extractiveness remains above pre-1986 baseline
 *   because actors learned tactical use of constitutional ambiguities.
 *
 * KEY AGENTS:
 *   - The Presidential Office: Primary beneficiary during unified government (institutional/arbitrage) — gains unilateral authority over foreign policy, defense, treaties. Constrained beneficiary during cohabitation (powerful/constrained).
 *   - The Parliamentary Majority: Primary beneficiary under domestic policy (institutional/arbitrage) — controls legislation and government formation. Victim during cohabitation (moderate/constrained) — constrained on foreign policy by presidential veto.
 *   - The Prime Minister: Institutional bridge actor (powerful/constrained) — gains domestic authority in cohabitation but loses foreign policy control. Performs coordination function but constrained by presidential veto.
 *   - The Minority Parliamentary Opposition: Trapped victim (powerless/trapped) — no legislative leverage in any regime; cannot exit constitutional framework; constrained by both majority and president.
 *   - The Constitutional Fifth Republic Framework: Institutional inertia (institutional/arbitrage) — persists despite functional degradation during cohabitation. Theater ratio masks jurisdictional ambiguities.
 *   - The Analytical Observer: Sees tangled rope structure globally (analytical/analytical) — recognizes both coordination (security coherence) and extraction (legislative suppression) functions simultaneously.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_presidential_authority, 0.52).
domain_priors:suppression_score(french_presidential_authority, 0.48).
domain_priors:theater_ratio(french_presidential_authority, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_presidential_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(french_presidential_authority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(french_presidential_authority, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_presidential_authority, tangled_rope).
narrative_ontology:human_readable(french_presidential_authority, "French Presidential Authority Under Cohabitation").
narrative_ontology:topic_domain(french_presidential_authority, "political/constitutional").

domain_priors:requires_active_enforcement(french_presidential_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_presidential_authority, presidential_office).
narrative_ontology:constraint_beneficiary(french_presidential_authority, unified_government_periods).
narrative_ontology:constraint_victim(french_presidential_authority, parliamentary_authority).
narrative_ontology:constraint_victim(french_presidential_authority, legislative_autonomy).
narrative_ontology:constraint_victim(french_presidential_authority, constitutional_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY PARLIAMENTARY OPPOSITION (SNARE) — Structurally trapped within a cohabitation regime where presidential veto power and legislative majority control both extractive mechanisms. The minority has no exit; constitutional amendment requires supermajority consensus. Suppression is high (institutional barriers prevent alternative constitutional arrangements). Extraction runs maximum toward this agent through both legislative paralysis and presidential override.
constraint_indexing:constraint_classification(french_presidential_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY MAJORITY DURING COHABITATION (TANGLED ROPE) — Benefits from legislative agenda-setting power (coordination function) while constrained by presidential veto on foreign policy, defense, and treaty ratification (extraction mechanism). Exit through dissolution is costly (risks new elections unfavorable to current majority). Both beneficiary and victim of the same institutional structure.
constraint_indexing:constraint_classification(french_presidential_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRESIDENTIAL OFFICE UNDER UNIFIED GOVERNMENT (ROPE) — Experiences authority as pure coordination: the constitution delegates foreign policy, defense, and treaty authority to the president, and a compatible prime minister executes domestic policy without conflict. No asymmetric extraction — presidential authority aligns with parliamentary will. Effective arbitrage through appointment and dissolution powers.
constraint_indexing:constraint_classification(french_presidential_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIME MINISTER DURING COHABITATION (TANGLED ROPE) — Coordinates domestic policy agenda (beneficiary function) while constrained by presidential authority over foreign/defense/treaty matters and presidential veto over key appointments (extraction mechanism). Exit through resignation risks political capital loss; constraining factors prevent unilateral action. Active enforcement required through cabinet negotiations and legislative/executive maneuvering.
constraint_indexing:constraint_classification(french_presidential_authority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL FRAMEWORK (PITON) — The Fifth Republic constitution (1958) was designed for a unified government where president and parliament align. Cohabitation (1986-1988, 1993-1995, 1997-2002) revealed the framework to be partially dysfunctional under divided government — the theatrical performance of constitutional roles (ceremonial presidency in cohabitation) obscures the lack of clear authority delegation. Theater ratio is high because both president and prime minister perform executive authority without clear separation of powers. The framework persists through institutional inertia despite functional degradation.
constraint_indexing:constraint_classification(french_presidential_authority, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a comparative constitutional perspective, French presidential authority embeds coordination (unified state direction in foreign policy) alongside extraction (suppression of legislative autonomy in treaty/defense domains). The system shows genuine coordination function (preventing parliamentary fragmentation on security matters) AND asymmetric extraction (executive dominance over legislative coequals). This classifies as tangled rope under analytical scrutiny.
constraint_indexing:constraint_classification(french_presidential_authority, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_presidential_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_presidential_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_presidential_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_presidential_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_presidential_authority, TR),
    TR >= 0.70.

:- end_tests(french_presidential_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Presidential authority extracts from the legislative domain through treaty and defense veto power, suppressing parliamentary autonomy in high-stakes decisions. However, the extraction is not maximal because the prime minister retains genuine domestic policy authority, and both offices coordinate on shared objectives during periods of compatible majority. The value reflects that extraction is domain-specific and oscillates with government composition. Suppression (0.48): Moderate. Constitutional amendment requires supermajority consensus (high barrier), but cohabitation remains possible and constitutional change is not physically impossible. Parliament cannot unilaterally restructure authority, but has fought for delineation (Laws clarifying cohabitation protocols). Theater ratio (0.58): Moderate-high. During unified government, theater is low (clear role separation). During cohabitation, both offices perform executive authority without explicit constitutional delineation, creating ceremonial performances ('presidential arbitrage,' 'prime ministerial governance'). Post-cohabitation theater remains elevated due to accumulated institutional learning about how to work ambiguities.
 *
 * PERSPECTIVAL GAP:
 *   The presidential office sees its authority as coordination (Rope during unified government, constrained Rope during cohabitation) because it solves the problem of preventing parliamentary fragmentation on security matters. The parliamentary majority sees authority as shared (Rope/Tangled Rope) when aligned with president, but constrained (Tangled Rope/Snare) when in opposition. The minority opposition sees pure extraction (Snare) — no legislative voice, no exit option, suppressed by both majority and president. The constitutional framework (Piton) sees itself degraded — the theatrical performance of cohabitation revealed the framework's functional gaps. The analytical observer sees tangled rope: genuine coordination function (preventing parliamentary fracture on security) plus asymmetric extraction (suppressing legislative voice on binding commitments).
 *
 * DIRECTIONALITY LOGIC:
 *   The presidential office benefits from constitutional delegation of foreign/defense/treaty authority, giving it arbitrage exit options (can act unilaterally in those domains). Directionality points beneficiary: low d, low χ. Parliamentary majority benefits from domestic authority but bears extraction through presidential veto on treaties and defense; during cohabitation, constrained exit (can dissolve parliament but at high cost) produces moderate d, moderate χ. Prime minister during cohabitation benefits from domestic agenda-setting (beneficiary on domestic coordination) while constrained by presidential veto on foreign policy (victim on foreign extraction) — balanced at d ≈ 0.50. Opposition minority faces trapped exit (no legislative options, cannot exit constitutional frame) as victim, producing high d, high χ. The constraint's directionality is not fixed across the political cycle — it shifts with government composition. The classification (Tangled Rope) holds because the underlying structure contains both coordination and extraction components regardless of which party holds which office.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_equilibrium_stability,
    'Is cohabitation an unstable equilibrium that will eventually collapse toward a unified-government norm, or a structurally sustainable dual-executive system?',
    'Long-term empirical tracking: (1) frequency of cohabitation recurrence post-2002 (two-term limit eliminated in 2007, changing equilibrium conditions); (2) constitutional amendment proposals and success rates; (3) strategic voting behavior to avoid divided government',
    'If unstable/regressing: presidential authority constraint is temporary (scaffold or piton with rising sunset probability). If sustainable: tangled rope classification is permanent structural feature of French politics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cohabitation_equilibrium_stability, empirical, 'Whether cohabitation is equilibrium or transient state').

omega_variable(
    treaty_veto_vs_legislative_authority,
    'Does presidential treaty authority represent genuine coordination (preventing parliament from fragmenting security matters) or pure executive extraction (suppressing legislative voice on binding commitments)?',
    'Comparative analysis: (1) instances where parliament opposed treaties president signed; (2) ratification voting patterns (unanimous vs contested); (3) post-treaty implementation parliamentary amendments or non-compliance; (4) cross-national comparison with other hybrid systems (German Bundestag, Italian parliament)',
    'If genuine coordination: beneficiary declaration should include ''state_security_coherence''; extraction component drops. If pure extraction: victims list should emphasize ''parliamentary_constitutional_authority''; extraction component rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_veto_vs_legislative_authority, empirical, 'Nature of presidential treaty authority (coordination vs extraction)').

omega_variable(
    dissolution_power_symmetry,
    'Does the president''s dissolution power (Article 12) function as mutual deterrent (symmetric constraint) or as a one-way extraction tool favoring the sitting president?',
    'Historical analysis: (1) dissolution outcomes vs expectations (did dissolved parliament return different majority or same?); (2) threat credibility during cohabitation (when was dissolution threat used, with what effect?); (3) whether dissolution creates mutual vulnerability or asymmetric advantage',
    'If symmetric deterrent: suppression value drops (both parties have exit/credible threat). If asymmetric: suppression value rises (parliament''s constraint is structural, president''s is tactical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissolution_power_symmetry, empirical, 'Dissolution power as mutual deterrent vs one-way tool').

omega_variable(
    domestic_policy_extraction_by_president,
    'To what degree does the presidential office extract concessions on domestic policy despite explicit prime ministerial authority through informal influence, media power, or agenda-setting?',
    'Longitudinal analysis of domestic policy disputes during cohabitation (1986-88, 1993-95, 1997-2002): (1) rate of government policy changes following presidential statements; (2) media framing attribution (does parliament get credit or president?); (3) Prime Minister public statements about constraint vs autonomy',
    'If significant informal extraction: base_extractiveness rises (0.52 → 0.58+). If minimal: domestic policy is genuine PM autonomy zone, beneficiaries list should exclude PM from extraction victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_policy_extraction_by_president, empirical, 'Degree of informal presidential extraction on domestic policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_presidential_authority, 1958, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frpres_theater_1958, french_presidential_authority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(frpres_theater_1986_cohabitation_onset, french_presidential_authority, theater_ratio, 2, 0.7).
narrative_ontology:measurement(frpres_theater_2000_post_cohabitation, french_presidential_authority, theater_ratio, 3, 0.58).

% Extraction over time
narrative_ontology:measurement(frpres_extract_1958, french_presidential_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(frpres_extract_1980, french_presidential_authority, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(frpres_extract_1986_cohabitation_onset, french_presidential_authority, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(frpres_extract_2000_post_cohabitation, french_presidential_authority, base_extractiveness, 3, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_presidential_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(french_presidential_authority, french_parliament_legislative_supremacy).
narrative_ontology:affects_constraint(french_presidential_authority, treaty_ratification_authority).
narrative_ontology:affects_constraint(french_presidential_authority, government_dissolution_power).

% DUAL FORMULATION NOTE:
% French presidential authority decomposes into three linked constraints: (1) treaty_ratification_authority (ε ≈ 0.45) — presidential power to sign and ratify treaties; (2) government_dissolution_power (ε ≈ 0.40) — presidential power to dissolve parliament; (3) cohabitation_framework (ε ≈ 0.52) — the institutional architecture that enables dual-executive dynamics when government and parliament diverge. Each has different empirical status and temporal trajectories. This story models the unified constraint (presidential authority) as a tangled rope whose apparent type depends on government composition. The network links indicate structural dependencies: treaty authority depends on maintaining executive voice (dissolution); dissolution power depends on treaty authority's legitimacy; cohabitation emerges when both are exercised simultaneously against legislative majority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_presidential_authority, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
