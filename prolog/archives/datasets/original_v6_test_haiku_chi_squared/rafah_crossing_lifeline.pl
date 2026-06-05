% ============================================================================
% CONSTRAINT STORY: rafah_crossing_lifeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rafah_crossing_lifeline, []).

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
 *   constraint_id: rafah_crossing_lifeline
 *   human_readable: The Rafah Medical Bottleneck
 *   domain: political/geopolitical/humanitarian
 *
 * SUMMARY:
 *   The Rafah Crossing serves as the primary (and during extended periods,
 *   sole) pathway for Palestinians requiring medical treatment beyond Gaza's
 *   degraded healthcare capacity. The constraint operates as a structural
 *   chokepoint: medical necessity is absolute (patients with cancer, cardiac
 *   conditions, complex surgeries cannot be treated locally), exit options
 *   are nonexistent (no alternative crossings reliably available for medical
 *   purposes), and control is externally held (Egypt and Israel determine
 *   access on a case-by-case basis through permit systems). This creates a
 *   Snare: high extractiveness (ε=0.68) from the perspective of patients and
 *   Palestinian institutions who bear the full cost of delays and denials;
 *   high suppression (0.78) through the permit apparatus and artificial
 *   scarcity; and moderate theater (0.55) as humanitarian rhetoric masks the
 *   underlying coercive structure. The constraint has intensified over the
 *   interval: extractiveness has risen from 0.45 to 0.68 as conflict has
 *   expanded and crossing reliability has declined; theater has risen from
 *   0.35 to 0.55 as international attention has focused on the 'lifeline'
 *   metaphor, increasing performative aspects while actual throughput remains
 *   constrained. From the perspectives of Israel and Egypt, the constraint
 *   appears as a coordination mechanism (Rope) that balances security and
 *   humanitarian concerns while maintaining state sovereignty. From the
 *   perspective of international humanitarian organizations, it appears as a
 *   Tangled Rope: they benefit from access (coordination) but face severe
 *   operational constraints (extraction). The constraint demonstrates how the
 *   same structural reality — a single-point bottleneck for life-or-death
 *   access — can be classified as Snare (victim perspective), Rope (control
 *   perspective), Tangled Rope (humanitarian organization perspective), and
 *   Piton (international discourse perspective).
 *
 * KEY AGENTS:
 *   - Palestinian patients requiring external medical care: Primary victims (powerless/trapped) — face permit delays, denial, and life-or-death triage based on crossing availability rather than medical urgency.
 *   - Gaza Health Ministry and NGO health providers: Secondary victims (moderate/constrained) — cannot build alternative infrastructure or import supplies reliably; operate through the bottleneck.
 *   - International humanitarian organizations (ICRC, MSF, UN): Mixed position (organized/constrained) — benefit from access and mandate to serve, but face severe operational constraints; have some advocacy power but limited sovereignty authority.
 *   - Egyptian government: Primary beneficiary-controller (institutional/arbitrage) — maintains dual legitimacy and cost control through permit management; has full exit options.
 *   - Israeli government: Primary beneficiary-controller (powerful/arbitrage) — maintains security authority and low-cost access filtering through the bottleneck; has full exit options.
 *   - International humanitarian discourse: Institutional norm carrier (institutional/arbitrage) — maintains rhetorical 'lifeline' framing that masks underlying coercion; benefits from continued narrative legitimacy.
 *   - Analytical observer: Neutral (analytical/analytical) — sees the snare structure as a deliberate architectural choice, not a humanitarian solution or natural constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rafah_crossing_lifeline, 0.68).
domain_priors:suppression_score(rafah_crossing_lifeline, 0.78).
domain_priors:theater_ratio(rafah_crossing_lifeline, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rafah_crossing_lifeline, extractiveness, 0.68).
narrative_ontology:constraint_metric(rafah_crossing_lifeline, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rafah_crossing_lifeline, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rafah_crossing_lifeline, snare).
narrative_ontology:human_readable(rafah_crossing_lifeline, "The Rafah Medical Bottleneck").
narrative_ontology:topic_domain(rafah_crossing_lifeline, "political/geopolitical/humanitarian").

domain_priors:requires_active_enforcement(rafah_crossing_lifeline).

% --- Structural relationships ---
narrative_ontology:constraint_victim(rafah_crossing_lifeline, palestinian_patients).
narrative_ontology:constraint_victim(rafah_crossing_lifeline, palestinian_healthcare_system).
narrative_ontology:constraint_victim(rafah_crossing_lifeline, humanitarian_medical_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN PATIENTS (SNARE) — Gaza patients requiring treatment beyond local capacity have no alternative exit: the Rafah crossing is the sole viable pathway for medical evacuation. Trapped by geography, blockade, and medical necessity. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.77. High extractiveness: patients face permit delays, throughput bottlenecks, and life-or-death triage based on crossing availability rather than medical urgency.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GAZA HEALTH MINISTRY / NGO PROVIDERS (SNARE) — Institutional actors within Gaza face constrained exit: they cannot create alternative medical infrastructure (Egyptian approval required), cannot import necessary equipment reliably, and must operate through the bottleneck. Constrained by Egyptian sovereignty and Israeli restrictions. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.71. Effective extraction through dependent institutional status.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL HUMANITARIAN ORGANIZATIONS (TANGLED ROPE) — Organizations like ICRC, MSF, and UN agencies benefit from access (coordination function: they provide care for underserved populations), but also face extraction through the constraint: permit dependencies, operational restrictions, and pressure to accept limited throughput as the cost of access. requires_active_enforcement=true; beneficiaries (themselves + local partners), victims (palestinian patients, medical field access). d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.54. Mixed: they have agency and global resources (organized), but operational capacity is severely constrained.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EGYPTIAN GOVERNMENT (ROPE) — Egypt has arbitrage exit options (can open/close, regulate flow, maintain dual legitimacy with Israel/Palestine). The constraint appears as coordination from Egypt's perspective: managing a complex triple-principal problem (Israeli security concerns, Palestinian medical necessity, Egyptian sovereignty). d≈0.10, f(d)≈0.05, σ=0.9 → χ≈0.03. Egypt experiences the crossing as a coordination mechanism, not extraction.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ISRAELI GOVERNMENT (ROPE) — Israel has full arbitrage options: can tighten/relax restrictions, negotiate terms, modify infrastructure. Experiences the constraint as coordination of security and humanitarian access. d≈0.08, f(d)≈-0.05, σ=1.0 → χ≈-0.04. Net beneficiary: the bottleneck serves as a control mechanism (low-cost filtering) that enables continued access provision while maintaining security authority.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HUMANITARIAN NORMS / INTERNATIONAL DISCOURSE (PITON) — The discourse about 'medical humanitarian corridors' and 'lifelines' persists in international rhetoric despite the structural reality that the Rafah crossing is a coercive bottleneck under sovereign control, not a genuine humanitarian mechanism. theater_ratio=0.55: there is substantive medical activity, but significant performative rhetoric ('lifeline,' 'humanitarian access') naturalizes what is actually a contingent extraction mechanism. The international norm language masks the snare structure.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the Rafah crossing represents a structurally coercive system: it concentrates life-or-death medical access into a single, externally controlled bottleneck. The constraint is not inherent to geography or medical necessity (SNARE, not MOUNTAIN) — it is the product of deliberate architectural choices (blockade, border closure, permit system). The analytical perspective confirms the snare classification across all parameters.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rafah_crossing_lifeline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rafah_crossing_lifeline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rafah_crossing_lifeline, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rafah_crossing_lifeline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rafah_crossing_lifeline, TR),
    TR >= 0.70.

:- end_tests(rafah_crossing_lifeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts at multiple levels: (1) medical outcome extraction — patients who could receive timely care elsewhere face delays and harm; (2) political extraction — Palestinian patients become leverage in sovereignty disputes; (3) resource extraction — Palestinians must spend scarce resources on permit procurement, travel, and informal payments; (4) dignity extraction — medical necessity becomes contingent on approval by external authorities. The value of 0.68 (vs. the extreme 0.85+) reflects that some humanitarian access does flow through, but under severe conditions. Suppression (0.78): Very high. The bottleneck operates through multiple suppression mechanisms: (1) permit system with opaque criteria; (2) limited daily throughput (Rafah crossing typically processes 100-200 patients/day against demand of 500+); (3) alternative pathways actively blocked or unavailable (northern crossings restricted, sea routes nonexistent, West Bank routing politically contentious); (4) information asymmetry about approval likelihood; (5) coercive conditions of exit (medical necessity eliminates real choice). Theater (0.55): Moderate. The crossing is substantively used for medical evacuation — it is not pure theater. However, international humanitarian rhetoric ('lifeline,' 'humanitarian corridor') significantly exceeds the actual humanitarian function, masking the underlying coercive structure. The theater has increased over time as international attention has focused attention on Rafah as a symbol of humanitarian access.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence across the constraint. Palestinian patients see a Snare: they are trapped and extraction is absolute. Gaza health institutions see a Snare: they cannot build alternatives and must accept whatever throughput the bottleneck provides. International humanitarian organizations see a Tangled Rope: they genuinely benefit from access (coordination) but face severe operational constraints (extraction); they have some agency through advocacy and resource mobilization, but cannot override the bottleneck itself. Egypt and Israel see a Rope: the constraint is a coordination mechanism that solves the hard problem of managing security and humanitarian access simultaneously. The international humanitarian norm discourse sees a Piton: the 'lifeline' rhetoric persists despite the degraded reality, maintained through institutional inertia and the absence of credible alternatives. The analytical observer sees a Snare: the structural reality is coercive, the humanitarian framing is theater, and the perspective divergence itself is evidence of the snare's extractive power (those who control it see coordination; those trapped by it see coercion).
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian patients: Victims + trapped → d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.77. Maximum extraction: absolute medical necessity eliminates exit options. Gaza institutions: Victims + constrained → d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.71. High extraction: institutional status allows some negotiation but cannot exit. International humanitarian organizations: Mixed (beneficiary + constrained) → d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.54. Moderate extraction: genuine coordination function (providing care) combined with operational constraints (permit dependencies). Egypt: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05, σ=0.9 → χ≈0.03. Net beneficiary: arbitrage exit options mean it experiences the constraint as a controlled mechanism. Israel: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.05, σ=1.0 → χ≈-0.04. Net beneficiary: the bottleneck serves security and political interests cost-free. Humanitarian discourse: Institutional/arbitrage → d≈0.05, f(d)≈-0.12 → χ≈-0.07. Beneficiary of the constraint's rhetorical value (the 'lifeline' metaphor provides legitimacy for current arrangements).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy RESOLVED: This constraint avoids the 'is it coordination or extraction?' collapse because the perspectival data is extremely asymmetric. ALL victim perspectives (patients, Gaza institutions, international humanitarian workers who see themselves as victims of operational limits) classify the constraint as Snare or (at minimum) Tangled Rope with severe extraction. ALL beneficiary perspectives (Israel, Egypt, international discourse) classify it as Rope. The divergence is not a matter of framing ambiguity — it reflects genuinely different structural positions. The snare classification is robust across all victim perspectives; the coordination framing from controller perspectives does NOT change the structural reality. The mandatrophy resolves in favor of Snare as the canonical constraint type because: (1) extractiveness (0.68) and suppression (0.78) clearly exceed rope thresholds; (2) the beneficiary group (Israeli and Egyptian state actors) differs radically from the victim group (patients with no exit options); (3) the constraint serves the interests of the controller at the expense of the trapped, which is the defining characteristic of extraction. The humanitarian-rhetoric perspective (Piton) confirms the snare structure: the fact that international discourse maintains a 'lifeline' framing despite the snare structure indicates theatrical maintenance of an extractive arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_access_vs_security_control,
    'Is the Rafah crossing fundamentally a humanitarian access mechanism or fundamentally a security control device? Can it be both?',
    'Historical analysis of crossing operations: correlation between security incidents and access restrictions; comparison of permit approval rates to medical urgency scores; examination of alternative access methods that have been explicitly rejected vs. those technically infeasible.',
    'If primarily humanitarian: may reclassify to Tangled Rope (coordination with extraction). If primarily security control: confirms Snare (extraction with humanitarian cover). If genuinely dual: Tangled Rope is appropriate, but theater_ratio should rise and suppression should drop as genuinely balanced coordination function becomes visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_access_vs_security_control, empirical, 'Whether crossing prioritizes humanitarian access or security control').

omega_variable(
    alternative_medical_pathways_technical_feasibility,
    'What is the technical and political feasibility of alternative medical pathways (Jerusalem hospitals with Palestinian integration, sea-based medical facilities, expanded Kerem Shalom cargo routes for medical supplies, West Bank patient routing)?',
    'Engineering feasibility studies; political negotiation analysis; documentation of why each alternative has been rejected or remained unimplemented; comparison to other conflict zones'' medical access solutions.',
    'If alternatives are technically and politically viable but deliberately blocked: strengthens snare classification (extraction through artificial scarcity). If alternatives are genuinely infeasible: may indicate the bottleneck is a structural necessity rather than a coercive choice, weakening snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_medical_pathways_technical_feasibility, empirical, 'Feasibility of alternative medical access pathways').

omega_variable(
    capacity_constraint_vs_artificial_throttling,
    'Is the Rafah crossing''s limited throughput driven by genuine infrastructure capacity constraints or by deliberate operational throttling (permit approvals, waiting periods, eligibility criteria)?',
    'Technical analysis of physical crossing capacity; comparison of actual daily permits issued vs. infrastructure maximum throughput; examination of permit approval decision-making criteria and variance; case studies of identical patients with different permit outcomes.',
    'If capacity-constrained: extraction is limited by objective scarcity (reduces snare severity, may suggest Tangled Rope with coordination elements). If artificially throttled: confirms deliberate coercion (strengthens snare, increases suppression metric).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_constraint_vs_artificial_throttling, empirical, 'Whether throughput is capacity-limited or artificially throttled').

omega_variable(
    patient_outcome_correlation_with_access_delays,
    'Do delayed medical evacuations through Rafah correlate with worse patient outcomes? What is the causal pathway from bottleneck to harm?',
    'Longitudinal medical outcome data for patients evacuated through Rafah vs. hypothetical on-time access; time-to-treatment analysis; documentation of cases where permit delays resulted in measurable harm or death.',
    'If strong correlation: confirms extraction impact (victims bear measurable harm). If weak correlation: may indicate the bottleneck constrains access but not medical outcomes in most cases, reducing the snare severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_outcome_correlation_with_access_delays, empirical, 'Correlation between access delays and patient harm').

omega_variable(
    institutional_sovereignty_vs_humanitarian_override,
    'Should humanitarian medical access override state sovereignty in controlling borders? Is the snare classification value-neutral or does it imply a mandate for international intervention?',
    'Comparative law analysis; precedent from other humanitarian corridors; international legal doctrine on humanitarian necessity vs. state sovereignty.',
    'If sovereignty is absolute: Snare is structural reality that may require acceptance rather than intervention. If humanitarian access overrides sovereignty: the Snare is illegitimate, and perspectives should shift (international community perspective may classify differently if it sees itself as bound by duty to override).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_sovereignty_vs_humanitarian_override, preference, 'Normative question of sovereignty vs. humanitarian override').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rafah_crossing_lifeline, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rafah_tr_t0, rafah_crossing_lifeline, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rafah_tr_t7, rafah_crossing_lifeline, theater_ratio, 7, 0.48).
narrative_ontology:measurement(rafah_tr_t15, rafah_crossing_lifeline, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(rafah_be_t0, rafah_crossing_lifeline, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rafah_be_t7, rafah_crossing_lifeline, base_extractiveness, 7, 0.62).
narrative_ontology:measurement(rafah_be_t15, rafah_crossing_lifeline, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rafah_crossing_lifeline, enforcement_mechanism).
narrative_ontology:affects_constraint(rafah_crossing_lifeline, gaza_healthcare_system_collapse).
narrative_ontology:affects_constraint(rafah_crossing_lifeline, israel_palestine_blockade_regime).
narrative_ontology:affects_constraint(rafah_crossing_lifeline, international_humanitarian_access_duty).

% DUAL FORMULATION NOTE:
% The Rafah crossing constraint is upstream of Gaza healthcare system collapse and downstream of the broader blockade regime. The crossing itself is a structural enforcement mechanism within a larger coercive system. Decomposition: the 'medical bottleneck' (this story, ε=0.68, Snare) is distinct from the 'existence of the Rafah crossing as infrastructure' (which might be Rope or Scaffold if functioning as genuine humanitarian access), but the actual operating pattern is snare-structured. The distinction hinges on whether the bottleneck is inherent to the crossing's capacity or is maintained through deliberate operational choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rafah_crossing_lifeline, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
