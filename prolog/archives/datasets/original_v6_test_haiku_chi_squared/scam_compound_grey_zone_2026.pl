% ============================================================================
% CONSTRAINT STORY: scam_compound_grey_zone_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scam_compound_grey_zone_2026, []).

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
 *   constraint_id: scam_compound_grey_zone_2026
 *   human_readable: The Southeast Asian Scam Compound Grey Zone
 *   domain: social/humanitarian/criminal
 *
 * SUMMARY:
 *   The Southeast Asian scam compound represents a structural collapse of the
 *   perpetrator-victim distinction, creating what authorities struggle to
 *   classify: is this human trafficking, organized fraud, debt-bondage, labor
 *   exploitation, or cybercrime? The answer is all simultaneously. Fortified
 *   compounds, typically in regions with weak governance and porous borders
 *   (Myanmar-Thailand border, Cambodia-Vietnam border, Philippines island
 *   interiors), have emerged as operational centers for large-scale romance
 *   fraud, pig-butchering schemes, cryptocurrency theft, and ransomware
 *   coordination. The constraint exhibits properties of a pure extraction
 *   mechanism (snare) from the perspective of international victims and
 *   trafficked workers, but takes on tangled-rope or even rope
 *   characteristics when viewed from compound management, corrupt officials,
 *   or coerced intermediate perpetrators. The structural ambiguity — is the
 *   worker a victim or a criminal? — is not accidental; it is the mechanism
 *   of extraction. By making victim status epistemically contested, the
 *   constraint suppresses external intervention (law enforcement
 *   uncertainty), external escape (victim shame/complicity), and internal
 *   organization (solidarity is complicated by shared guilt). Theater ratio
 *   (0.55) reflects moderate performance: compound operations are genuinely
 *   functional (scams produce real financial flows), but significant effort
 *   is directed toward concealing the operation from authorities and victims.
 *
 * KEY AGENTS:
 *   - Captive Workers: Primary victims (powerless/trapped) — trafficked via false employment or romantic promises; document confiscated; subjected to debt-bondage, violence threats, psychological coercion; zero unilateral exit options
 *   - Romance/Pig-Butchering Victims: Primary external victims (powerless/trapped) — emotionally and financially drained across borders; suppressed by shame and sunk-cost psychology; cannot identify the operational structure enabling the scam
 *   - Coerced Scammers: Intermediate status (moderate/constrained) — began as recruits promised wages, now debt-bonded; simultaneously perpetrators of fraud and victims of trafficking; benefit from compound infrastructure while being extracted from
 *   - Compound Management/Operators: Primary beneficiary (institutional/arbitrage) — profit from labor extraction and scam coordination; experience constraint as operational efficiency
 *   - Corrupt Local Officials: Secondary beneficiary (organized/mobile) — extract regular bribes in exchange for non-enforcement; benefit from constraint's persistence but could theoretically mobilize
 *   - Digital Platforms: Indirect beneficiary (institutional/arbitrage) — reduced friction on dating apps, messaging platforms, crypto exchanges enables scam scaling; business model indirectly optimizes for scammer convenience
 *   - Law Enforcement / NGO Responders: Constrained responder (moderate/constrained) — face jurisdictional fragmentation, corruption at scale, identification ambiguity (is resident victim or criminal?); cannot intervene without clarifying status
 *   - Global Financial System: Abstract victim (analytical/analytical) — trust and integrity in digital payments/identity systems degraded by large-scale fraud; cannot opt out of participation without opting out of modernity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scam_compound_grey_zone_2026, 0.68).
domain_priors:suppression_score(scam_compound_grey_zone_2026, 0.78).
domain_priors:theater_ratio(scam_compound_grey_zone_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scam_compound_grey_zone_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(scam_compound_grey_zone_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(scam_compound_grey_zone_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scam_compound_grey_zone_2026, snare).
narrative_ontology:human_readable(scam_compound_grey_zone_2026, "The Southeast Asian Scam Compound Grey Zone").
narrative_ontology:topic_domain(scam_compound_grey_zone_2026, "social/humanitarian/criminal").

domain_priors:requires_active_enforcement(scam_compound_grey_zone_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scam_compound_grey_zone_2026, compound_management).
narrative_ontology:constraint_beneficiary(scam_compound_grey_zone_2026, supply_chain_actors).
narrative_ontology:constraint_beneficiary(scam_compound_grey_zone_2026, corrupt_officials).
narrative_ontology:constraint_victim(scam_compound_grey_zone_2026, captive_workers).
narrative_ontology:constraint_victim(scam_compound_grey_zone_2026, foreign_victims_of_scams).
narrative_ontology:constraint_victim(scam_compound_grey_zone_2026, source_country_nationals).
narrative_ontology:constraint_victim(scam_compound_grey_zone_2026, global_financial_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTIVE WORKER (SNARE) — Trafficked into compound, document confiscated, death threats against family. Zero exit options; faces maximum extraction. d≈0.98, f(d)≈1.44, σ=0.8 → χ≈0.76. The constraint exists solely to extract labor and prevent escape.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ROMANCE SCAM VICTIM (SNARE) — Located outside compound, financially drained by fabricated emotional relationship. Suppressed by shame, incomplete information about compound structure. Trapped by sunk costs and psychological manipulation. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.93. Global scope amplifies extraction across jurisdictions.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: COERCED SCAMMER (TANGLED ROPE) — May have begun as recruit promised wages, now coerced via debt-bondage and violence threats. Simultaneously victim and perpetrator; benefits from compound's infrastructure (shelter, meals, safety from local authorities) while being extracted from. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.46. Coordination function (operational scam machinery) paired with extraction (bondage).
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMPOUND MANAGEMENT (ROPE) — Views constraint as coordination mechanism: organizing labor, managing supply chains, maintaining operational security. Experiences extraction as efficient resource allocation. d≈0.08, f(d)≈-0.11, σ=0.9 → χ≈-0.07. Net beneficiary; perceives system as functional optimization.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: CORRUPT OFFICIALS (TANGLED ROPE) — Extract bribes while providing operational security; benefit from extraction but also depend on compound's functioning. d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.38. Mixed beneficiary-victim status; could mobilize enforcement but choose not to.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: LAW ENFORCEMENT / NGO RESPONDERS (SNARE) — Face jurisdictional fragmentation, corruption at scale, resource constraints. Constrained by victim identification ambiguity: compound residents are simultaneously trafficked and criminal. d≈0.68, f(d)≈1.02, σ=1.2 → χ≈0.68. Extraction mechanism is structural inability to intervene effectively across borders and corrupted local governance.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Abstract collective good (global financial system integrity, trust in digital platforms). Trapped by architectural asymmetry: extraction mechanisms are cheaper to scale than detection mechanisms. d≈0.85, f(d)≈1.28, σ=1.2 → χ≈0.87. Cannot opt out of participation in global financial flows without opting out of modernity.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_compound_grey_zone_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scam_compound_grey_zone_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(scam_compound_grey_zone_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint generates substantial monetary extraction from international victims (estimates: $10-20B USD annually for Southeast Asian compounds). Labor extraction from captive workers is severe (unpaid, coerced, no exit). The metric reflects both the volume of extraction and its structural necessity to compound operations. However, it is not maximal (0.70+) because some operational functions are genuine coordination (scam networks require real division of labor, real operational infrastructure), not pure rent-seeking. Suppression (0.78): Very high. Multiple suppression mechanisms operate simultaneously: (1) Victim suppression: captive workers face violence threats against families in source countries; external victims suppressed by shame and self-blame; (2) Epistemic suppression: victim-perpetrator boundary is intentionally blurred, confusing potential rescuers; (3) Structural suppression: weak regional governance provides minimal alternative protection; (4) Organizational suppression: compound residents cannot credibly threaten collective action (coerced scammers' status as perpetrators undermines solidarity). Theater ratio (0.55): Moderate-low and declining. Compound operations are not primarily performative; they produce real financial output. But significant theater persists: false identities for romance/dating profiles, fabricated victim narratives, pseudo-documentation for visa processes. The decline over 10 years reflects increasing operational efficiency (less need for elaborate deception as scam pipelines mature and victims become self-selecting through platform algorithms) and possibly increased sophistication (AI-generated romance narratives reduce manual theater cost).
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is the constraint itself. Compound management sees coordination (Rope): efficient organization of labor and capital. Coerced scammers see mixed extraction-coordination (Tangled Rope): they benefit from the compound's provision of survival, but are extracted from via coercion. Captive workers see pure extraction (Snare): no benefits, only costs. Romance victims see pure extraction (Snare): financial and emotional harm with no offsetting benefit. Law enforcement sees tangled complexity (Snare from their constrained perspective): they understand the extraction occurs but cannot intervene without clarifying victim status. Corrupt officials see coordination and benefit (Rope trending toward arbitrage): bribery is straightforward extraction, but they depend on compound functioning. The global financial system sees snare: the constraint undermines trust in digital infrastructure globally. The analytical observer risks naturalizing the constraint as inevitable consequence of weak governance (mountain view), but the structural data reveals it as a deliberately maintained extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Captive workers: Victim + trapped → d≈0.98, f(d)≈1.44. Maximum extraction. Romance victims: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction; trapped by psychological factors (sunk cost, shame) rather than physical confinement, but equally unable to exit. Coerced scammers: Mixed victim-beneficiary + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; they receive some benefits (survival, shelter) but are coerced via debt and violence. Compound management: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; no extraction from their perspective. Corrupt officials: Beneficiary + mobile → d≈0.35, f(d)≈0.28. Low-moderate extraction; they benefit significantly but could theoretically mobilize to stop if incentives shifted. Law enforcement: Observer + constrained → d≈0.68, f(d)≈1.02. Moderate-high extraction from their perspective (structural inability to act). Global financial system: Observer + analytical → d≈0.85, f(d)≈1.28. High extraction; trapped in global architecture.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA DOMINANT SNARE: The constraint classifies as Snare (extractiveness=0.68, suppression=0.78) across the majority of structural perspectives. The tangled-rope perspectives (coerced scammers, corrupt officials) exist but are secondary to the dominant snare classification. This resolves mandatrophy by clarifying: (1) The coordination function in the compound (scam operations) is not the constraint itself — it is the mechanism used to implement the extraction. (2) The constraint is defined by the structure of suppression and extraction, not by the existence of organization. (3) The apparent ambiguity (perpetrator-victim collapse) is itself the extraction mechanism. By making status contested, the constraint suppresses intervention. The mandatrophy resolution confirms: this is not coordination falsely labeled as extraction (Rope misclassified as Snare). This is extraction (Snare) that uses organizational structures (real scam operations) to function. The organizational reality does not elevate it to Tangled Rope because the coerced workers and victims have no genuine coordination benefit — they are not co-solving a problem, they are being separated from resources. The beneficiaries (compound management, corrupt officials, platform business models) gain coordination benefits, but the constraint from the victim perspective is pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    perpetrator_victim_boundary_collapse,
    'At what point of coercion does a scammer become a trafficked victim, and does this distinction matter for intervention strategy?',
    'Longitudinal interview data with compound escapees; psychological assessment of trauma bonding vs. rational cost-benefit analysis; comparison of behavioral patterns with non-coerced criminal perpetrators',
    'If boundary is clear: classic trafficking narrative applies, rescue/reintegration strategies can follow. If boundary is collapsed: rescue becomes problematic (removing someone from survival situation), criminal prosecution becomes ethically fraught, victim support programs must serve dual status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetrator_victim_boundary_collapse, empirical, 'Whether perpetrator-victim distinction can be operationalized in compound context').

omega_variable(
    compound_exit_feasibility,
    'Given debt-bondage, family coercion, and local official corruption, is any resident''s exit option genuinely ''mobile'' or are all residents structurally ''trapped''?',
    'Case analysis of successful escapes; mapping of escape routes and their cost-benefit profiles; interviews with residents about perceived vs. actual exit constraints',
    'If exit is possible: some coerced scammers might be seen as constrained (mobile in principle). If exit is effectively impossible: all residents are trapped, classification is uniformly snare across all victim categories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compound_exit_feasibility, empirical, 'Whether trapped classification is universal or varies by resident type').

omega_variable(
    weak_governance_naturalization,
    'Is the compound a natural consequence of weak state capacity in the region, or a deliberately cultivated extraction mechanism that worsens state capacity?',
    'Comparative analysis: weak governance regions with and without scam compounds; timeline of compound growth vs. governance institution degradation; interview data on official knowledge and deliberate non-enforcement',
    'If natural consequence: constraint appears mountain-like (immutable absent state capacity). If deliberate mechanism: reveals snare structure maintained by organized corruption, enabling targeted intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_governance_naturalization, empirical, 'Whether weak governance enables or is worsened by compound structure').

omega_variable(
    platform_architectural_responsibility,
    'Do digital platforms (dating apps, cryptocurrency exchanges, messaging platforms) that enable scam coordination bear responsibility as beneficiaries, or are they neutral conduits?',
    'Structural analysis of platform friction costs: what would be required to block scam patterns? User behavior analysis showing platform designs that accidentally optimize for scammer convenience. Comparison with platforms that have implemented costly anti-scam measures.',
    'If neutral: platforms are victims themselves (analytical observer narrative). If beneficiary: platforms'' business models tacitly depend on reduced friction for scam operations; classification shifts toward organized institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_architectural_responsibility, conceptual, 'Platform role as neutral conduit vs. architectural beneficiary').

omega_variable(
    ransomware_victim_overlap,
    'What fraction of scam compound residents are themselves running ransomware operations, and how does this alter victim-perpetrator classification?',
    'Compound operational mapping; interviews with law enforcement cyber units; cross-reference of ransomware attack attribution with compound location data; analysis of victim harm (ransomware victims vs. scam victims) as basis for differentiated justice',
    'If negligible overlap: scam compound is primarily romance/romance-adjacent fraud. If significant: compound residents are dual-status (victims of trafficking AND perpetrators of high-harm cybercrime), complicating both rescue and prosecution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ransomware_victim_overlap, empirical, 'Extent of ransomware operations within scam compounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scam_compound_grey_zone_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scam_tr_t0, scam_compound_grey_zone_2026, theater_ratio, 0, 0.62).
narrative_ontology:measurement(scam_tr_t5, scam_compound_grey_zone_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(scam_tr_t10, scam_compound_grey_zone_2026, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(scam_be_t0, scam_compound_grey_zone_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(scam_be_t5, scam_compound_grey_zone_2026, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(scam_be_t10, scam_compound_grey_zone_2026, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scam_compound_grey_zone_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(scam_compound_grey_zone_2026, cross_border_labor_trafficking).
narrative_ontology:affects_constraint(scam_compound_grey_zone_2026, romance_fraud_victim_identification).
narrative_ontology:affects_constraint(scam_compound_grey_zone_2026, platform_moderation_asymmetry).
narrative_ontology:affects_constraint(scam_compound_grey_zone_2026, weak_state_capacity_feedback).

% DUAL FORMULATION NOTE:
% The scam compound grey zone is a structural consequence of and contributor to weak governance. As a separate constraint story, cross-border labor trafficking has higher ε (0.45) reflecting legitimate labor movement; the compound grey zone adds extraction mechanisms (coercion, debt-bondage, victim-perpetrator ambiguity) raising ε to 0.68. The network link shows that the compound constraint is downstream of weak governance but creates feedback loops that further degrade state capacity. Similarly, the compound amplifies romance fraud's impact; the individual romance scam may have ε=0.38, but the compound's industrial scaling and victim suppression raises the systemic constraint to ε=0.68.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scam_compound_grey_zone_2026, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
