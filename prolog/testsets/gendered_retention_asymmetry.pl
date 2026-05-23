% ============================================================================
% CONSTRAINT STORY: gendered_retention_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_retention_asymmetry, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_retention_asymmetry
 *   human_readable: Gendered Retention Asymmetry in Clinical Medicine
 *   domain: healthcare_workforce_economics/organizational_sociology/gender_studies
 *
 * SUMMARY:
 *   The gendered retention asymmetry in clinical medicine creates a
 *   systematic 3-year career length differential (9 years women vs 12 years
 *   men) driven by unequal distribution of unpaid caregiving labor and
 *   organizational failure to accommodate reproductive and caregiving needs.
 *   Women physicians cite childcare as exit reason at 5x the rate of male
 *   colleagues (21.3% vs 4.2%). The constraint operates through identity lock
 *   rather than material trap: women physicians have medical degrees and
 *   earning capacity (structurally mobile) but experience exit as abandoning
 *   core identity because both professional role and caregiving role are
 *   identity-constituting. Organizations benefit by externalizing caregiving
 *   costs, avoiding accommodation investments (onsite childcare, paid
 *   parental leave, lactation facilities, schedule flexibility), and
 *   capturing the career output of physicians with maximum availability
 *   (predominantly men with traditional family structures). The theater ratio
 *   (0.58) reflects performative accommodation: diversity statements,
 *   mentorship programs, and women-in-medicine initiatives that do not
 *   address the structural binding mechanism. The constraint is downstream of
 *   administrative_extraction_mechanism: the same organizational structures
 *   that extract through administrative burden also extract through
 *   caregiving externalization, and both mechanisms disproportionately affect
 *   women physicians.
 *
 * KEY AGENTS:
 *   - Women Physicians with Caregiving: Primary victim (powerless/identity_locked) — bears full cost of unequal caregiving distribution plus career penalty; identity constituted through both roles
 *   - Part-Time Physicians: Secondary victim (moderate/constrained) — reduced partnership access and compensation but some flexibility; pathway reinforces asymmetry
 *   - Male Physicians Traditional Family: Primary beneficiary (institutional/arbitrage) — partner absorbs caregiving, enabling uninterrupted career progression
 *   - Healthcare Organizations Avoiding Accommodation: Primary beneficiary (institutional/arbitrage) — externalize caregiving costs, avoid accommodation investment
 *   - Gender Equity Coalition: Organized agents (organized/mobile) — building alternative structures with sunset logic through policy mandates and norm shifts
 *   - Male Physicians Seeking Caregiving: Secondary victim (moderate/constrained) — face career penalty for accommodation but smaller and less identity-threatening than women
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies structural extraction through organizational design choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_retention_asymmetry, 0.68).
domain_priors:suppression_score(gendered_retention_asymmetry, 0.72).
domain_priors:theater_ratio(gendered_retention_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_retention_asymmetry, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_retention_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gendered_retention_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_retention_asymmetry, snare).
narrative_ontology:human_readable(gendered_retention_asymmetry, "Gendered Retention Asymmetry in Clinical Medicine").
narrative_ontology:topic_domain(gendered_retention_asymmetry, "healthcare_workforce_economics/organizational_sociology/gender_studies").

domain_priors:requires_active_enforcement(gendered_retention_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(gendered_retention_asymmetry, implicit).
narrative_ontology:cs_authority_grounding(gendered_retention_asymmetry, practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_retention_asymmetry, male_physicians).
narrative_ontology:constraint_beneficiary(gendered_retention_asymmetry, traditional_organizational_structures).
narrative_ontology:constraint_beneficiary(gendered_retention_asymmetry, healthcare_systems_avoiding_accommodation_costs).
narrative_ontology:constraint_victim(gendered_retention_asymmetry, women_physicians).
narrative_ontology:constraint_victim(gendered_retention_asymmetry, healthcare_workforce_diversity).
narrative_ontology:constraint_victim(gendered_retention_asymmetry, patient_populations_requiring_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN PHYSICIAN WITH CAREGIVING (SNARE) — Identity-locked rather than materially trapped: has medical degree and earning capacity (structurally mobile) but identity is constituted through both professional role and caregiving role. Exit from clinical practice feels like abandoning core identity. Organizational structures offer no accommodation, forcing binary choice between career and caregiving. Maximum extraction: bears full cost of unequal caregiving distribution plus career penalty for attempting both.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: PART-TIME PHYSICIAN (TANGLED ROPE) — Constrained by reduced partnership track access, lower compensation per hour, and professional marginalization, but benefits from some schedule flexibility and continued clinical practice. Mixed extraction: the part-time pathway enables continued work but embeds career penalties and reinforces the gender asymmetry by treating accommodation as individual deviation rather than structural norm.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALE PHYSICIAN TRADITIONAL FAMILY (ROPE) — Benefits from unequal caregiving distribution: partner absorbs domestic labor, enabling uninterrupted career progression. Experiences constraint as pure coordination: the system rewards continuous availability, which this agent can provide. Net beneficiary of the asymmetry.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTHCARE ORG AVOIDING ACCOMMODATION (ROPE) — Institutional beneficiary. Avoids costs of onsite childcare, paid parental leave, lactation facilities, and schedule flexibility by externalizing caregiving burden to individual physicians (predominantly women). Experiences constraint as coordination: the system selects for physicians with maximum availability, which traditional structures provide. Extraction flows toward this agent.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: GENDER EQUITY COALITION (SCAFFOLD) — Organized agents (medical associations, advocacy groups, policy coalitions) building alternative structures: paid parental leave mandates, onsite childcare requirements, part-time partnership tracks, lactation facility standards. See the asymmetry as temporary coordination failure with sunset: as accommodation becomes standard practice and younger male physicians demand caregiving time, the binary choice dissolves. Estimated sunset: 15-25 years as generational norms shift and policy mandates accumulate.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: MALE PHYSICIAN SEEKING CAREGIVING (TANGLED ROPE) — Constrained by professional norms that penalize caregiving regardless of gender, but benefits from lower baseline expectation (4.2% cite childcare as exit reason vs 21.3% for women). Mixed extraction: faces career penalty for accommodation requests but penalty is smaller and less identity-threatening than for women colleagues. This perspective reveals the constraint extracts from caregiving regardless of gender, but asymmetrically.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global perspective, the constraint is extractive: organizations externalize caregiving costs onto individual physicians, predominantly women, through structural failure to accommodate reproductive and caregiving needs. The 3-year median career length gap (9 vs 12 years) and 5x differential in childcare-driven exit (21.3% vs 4.2%) are not natural outcomes but products of organizational design choices that benefit from unequal caregiving distribution. High extraction, high suppression, active enforcement through partnership track requirements and schedule inflexibility.
constraint_indexing:constraint_classification(gendered_retention_asymmetry, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_retention_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gendered_retention_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gendered_retention_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_retention_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_retention_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Organizations capture career output from physicians with maximum availability while externalizing caregiving costs onto individual physicians, predominantly women. The 3-year career length differential and 5x childcare exit differential are not natural outcomes but products of organizational design. The extraction has increased over the 30-year interval as medical training costs have risen (making early exit more costly) while accommodation investments have remained minimal. Suppression (0.72): High. Binding mechanisms include identity fusion (professional and caregiving roles both identity-constituting), partnership track requirements demanding continuous full-time availability, professional norms penalizing accommodation requests, lack of structural alternatives (part-time tracks are marginal and penalized), and economic sunk costs of medical training. Suppression is cognitive (identity lock) rather than purely material, making it more persistent. Theater ratio (0.58): Moderate-high. Performative accommodation through diversity statements, mentorship programs, women-in-medicine initiatives, and symbolic leadership appointments that do not address structural binding mechanisms. The theater has increased as organizations have adopted diversity rhetoric while avoiding accommodation costs. Genuine accommodation (onsite childcare, paid parental leave, lactation facilities, normalized part-time partnership tracks) remains rare.
 *
 * PERSPECTIVAL GAP:
 *   The woman physician with caregiving responsibilities experiences pure extraction (Snare) — forced binary choice between career and caregiving with no accommodation, identity-locked by fusion of both roles. The part-time physician experiences mixed coordination and extraction (Tangled Rope) — the pathway enables continued work but embeds career penalties and reinforces asymmetry. The male physician with traditional family structure experiences pure coordination (Rope) — the system rewards continuous availability, which unequal caregiving distribution enables this agent to provide. The healthcare organization avoiding accommodation experiences pure coordination (Rope) — externalize caregiving costs and select for maximum-availability physicians. The gender equity coalition sees temporary problem with sunset (Scaffold) — accommodation is becoming standard practice through policy mandates and generational norm shifts. The male physician seeking caregiving time experiences mixed coordination and extraction (Tangled Rope) — faces career penalty but smaller than women colleagues. The analytical observer identifies pure extraction (Snare) — organizational design choices that benefit from unequal caregiving distribution. The perspectival gap reveals that beneficiaries experience the constraint as natural coordination while victims experience it as extractive trap, and the analytical view identifies the extraction mechanism that beneficiary perspectives naturalize.
 *
 * DIRECTIONALITY LOGIC:
 *   Women physicians with caregiving responsibilities are primary victims with identity_locked exit: structurally mobile (medical degree, earning capacity) but functionally trapped by identity fusion with both professional and caregiving roles. The engine derives high d (victim + identity_locked) producing high experienced extraction. Part-time physicians are victims with constrained exit: face career penalties but retain some agency and benefit from continued practice. Male physicians with traditional family structures are beneficiaries with arbitrage exit: partner absorbs caregiving, enabling uninterrupted career progression. Healthcare organizations avoiding accommodation are beneficiaries with arbitrage exit: externalize caregiving costs and capture output from maximum-availability physicians. The gender equity coalition has organized power and mobile exit: building alternative structures and can operate across multiple healthcare systems. Male physicians seeking caregiving time are victims with constrained exit: face career penalty but smaller than women colleagues, revealing the constraint extracts from caregiving regardless of gender but asymmetrically. The analytical observer uses analytical power and exit to identify the structural extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by demonstrating that the career length differential and childcare exit differential are not natural outcomes of biological difference or individual choice but products of organizational design choices that benefit from unequal caregiving distribution. The coordination function (selecting for physicians with maximum availability) is real but the extraction mechanism (externalizing caregiving costs onto individual physicians, predominantly women, through structural failure to accommodate) is separable and measurable. The 5x differential in childcare-driven exit (21.3% vs 4.2%) and the 3-year career length gap (9 vs 12 years) quantify the extraction. The part-time pathway reveals the hybrid structure: it provides some coordination (continued clinical practice) but embeds extraction (career penalties that exceed coordination costs). The male physician seeking caregiving time perspective reveals the constraint extracts from caregiving regardless of gender but asymmetrically, confirming the mechanism is organizational structure rather than biological necessity. The scaffold perspective (gender equity coalition) identifies the sunset mechanism: as accommodation becomes standard practice and younger male physicians demand caregiving time, the binary choice dissolves. The analytical classification (Snare) is justified by high extraction (0.68), high suppression (0.72), and active enforcement through partnership track requirements and schedule inflexibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_cost_threshold,
    'At what cost threshold do organizational accommodation investments (onsite childcare, paid leave, schedule flexibility) become economically rational for healthcare systems?',
    'Cost-benefit analysis comparing accommodation investment vs physician recruitment/training costs multiplied by differential retention rates; longitudinal data from systems that have implemented full accommodation packages',
    'If threshold is lower than current practice suggests, organizations are leaving value on table and the constraint is pure extraction. If threshold is higher, some extraction may be economically rational coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_cost_threshold, empirical, 'Economic threshold for accommodation investment rationality').

omega_variable(
    generational_norm_shift_timeline,
    'Will younger male physicians demand caregiving time at rates sufficient to dissolve the gender asymmetry, or will traditional norms persist?',
    'Longitudinal survey data on caregiving time preferences and actual time allocation by physician cohort and gender; tracking of part-time requests and parental leave utilization by gender over 10-year intervals',
    'If male demand for caregiving time rises to match female demand, the scaffold perspective is confirmed and sunset is real. If asymmetry persists across generations, the constraint is structural rather than transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_norm_shift_timeline, empirical, 'Whether generational shift will equalize caregiving demand').

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the primary binding mechanism for women physicians cognitive (identity fusion with both professional and caregiving roles) or material (economic dependency, lack of alternative careers)?',
    'Post-exit trajectory analysis: do physicians who leave clinical practice for caregiving reasons return when caregiving demands decrease? Comparison of exit rates between high-earning specialists (low economic dependency) and primary care physicians (higher economic pressure).',
    'If identity-locked, the constraint''s effective suppression is higher than structural measures suggest because the lock persists after material barriers are removed. If materially trapped, accommodation investments have immediate effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Primary mechanism binding women physicians to binary choice').

omega_variable(
    part_time_penalty_magnitude,
    'What proportion of the part-time physician''s career penalty is coordination cost (genuine efficiency loss from reduced availability) vs extractive overhead (organizational punishment for deviation from full-time norm)?',
    'Productivity analysis controlling for hours worked; comparison of per-hour compensation and partnership track access between part-time and full-time physicians with equivalent patient outcomes and satisfaction scores',
    'If penalty exceeds coordination cost, the part-time pathway is itself an extraction mechanism. If penalty matches coordination cost, it represents fair pricing of reduced availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(part_time_penalty_magnitude, empirical, 'Proportion of part-time penalty that is extractive vs coordinative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_retention_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gra_theater_1990, gendered_retention_asymmetry, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gra_theater_2000, gendered_retention_asymmetry, theater_ratio, 10, 0.52).
narrative_ontology:measurement(gra_theater_2010, gendered_retention_asymmetry, theater_ratio, 20, 0.58).
narrative_ontology:measurement(gra_theater_2020, gendered_retention_asymmetry, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(gra_extract_1990, gendered_retention_asymmetry, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gra_extract_2000, gendered_retention_asymmetry, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gra_extract_2010, gendered_retention_asymmetry, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(gra_extract_2020, gendered_retention_asymmetry, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_retention_asymmetry, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of administrative_extraction_mechanism: the same organizational structures that extract through administrative burden also extract through caregiving externalization. Both mechanisms disproportionately affect women physicians and both operate through structural failure to accommodate rather than through explicit policy. The gendered retention asymmetry has its own extractiveness value (0.68) reflecting the career length differential and childcare exit differential; the administrative extraction mechanism has its own extractiveness reflecting the documentation burden and time theft. They are structurally distinct constraints that share organizational beneficiaries and victim populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
