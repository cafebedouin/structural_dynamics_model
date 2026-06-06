% ============================================================================
% CONSTRAINT STORY: gendered_caregiving_penalty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_caregiving_penalty, []).

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
 *   constraint_id: gendered_caregiving_penalty
 *   human_readable: Gendered Caregiving Penalty in Medical Workforce
 *   domain: health_workforce_economics/organizational_behavior/gender_labor
 *
 * SUMMARY:
 *   The gendered caregiving penalty in medicine extracts heavily from women
 *   physicians through a structural mechanism: unpaid caregiving labor falls
 *   disproportionately on women, while medical institutions maintain
 *   ideal-worker norms (unlimited availability, no caregiving
 *   responsibilities) that are incompatible with primary caregiving. The
 *   constraint operates through identity lock rather than material trap:
 *   women physicians have high earning capacity and structural mobility, but
 *   their professional identity (physician) and caregiving identity (mother)
 *   are both constitutive, and the constraint forces a choice between them.
 *   The observable delta is stark: 21.3% of women physicians exit for
 *   childcare reasons vs 4.2% of men; median clinical career length is 9
 *   years for women vs 12 years for men; 7.9% of women exit specifically for
 *   family care vs 0.6% of men. The constraint's theater ratio (0.58)
 *   reflects the gap between formal accommodation policies (parental leave,
 *   part-time tracks) and actual usability: policies exist on paper but are
 *   rendered unusable by coverage gaps, career penalties, and cultural norms
 *   that treat accommodation as lack of commitment. The constraint has
 *   increased in both extractiveness and theater over the 50-year interval as
 *   women's representation in medicine grew (creating more victims) while
 *   institutional structures remained unchanged, and as formal accommodation
 *   policies proliferated without reducing the underlying penalty structure.
 *
 * KEY AGENTS:
 *   - Women Physicians with Young Children: Primary victim (powerless/identity_locked) — bears extraction through forced exit, career penalty, or burnout; identity constituted through both professional and caregiving roles
 *   - Women Physicians in Training: Secondary victim (powerless/constrained) — faces specialty choice constrained by future caregiving penalty; suppression operates through debt, prestige hierarchies, and institutional culture
 *   - Healthcare Institutions: Primary beneficiary (institutional/arbitrage) — benefits from ideal-worker norm through free disposal of trained physicians, avoidance of accommodation costs, and reduced competition for leadership positions
 *   - Male Physicians: Secondary beneficiary (powerful/mobile) — benefits from reduced competition and from gendered division of labor that subsidizes availability
 *   - Part-Time Women Physicians: Mixed position (moderate/constrained) — has negotiated accommodation but pays career penalty; both beneficiary (has caregiving time) and victim (pays professional cost)
 *   - Medical Workforce Equity Coalition: Organized agents (organized/constrained) — sees constraint as temporary with generational sunset logic; advocates for structural change but has constrained exit
 *   - Healthcare System Capacity: Abstract victim (powerless/trapped) — loses trained physician capacity through gendered exit pattern; no advocate and no exit option
 *   - Patient Continuity of Care: Abstract victim (powerless/trapped) — disrupted by physician exit and turnover; bears cost of institutional failure to accommodate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_caregiving_penalty, 0.68).
domain_priors:suppression_score(gendered_caregiving_penalty, 0.72).
domain_priors:theater_ratio(gendered_caregiving_penalty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_caregiving_penalty, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_caregiving_penalty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gendered_caregiving_penalty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_caregiving_penalty, snare).
narrative_ontology:human_readable(gendered_caregiving_penalty, "Gendered Caregiving Penalty in Medical Workforce").
narrative_ontology:topic_domain(gendered_caregiving_penalty, "health_workforce_economics/organizational_behavior/gender_labor").

domain_priors:requires_active_enforcement(gendered_caregiving_penalty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_caregiving_penalty, healthcare_institutions).
narrative_ontology:constraint_beneficiary(gendered_caregiving_penalty, male_physicians).
narrative_ontology:constraint_beneficiary(gendered_caregiving_penalty, medical_training_programs).
narrative_ontology:constraint_victim(gendered_caregiving_penalty, women_physicians).
narrative_ontology:constraint_victim(gendered_caregiving_penalty, healthcare_system_capacity).
narrative_ontology:constraint_victim(gendered_caregiving_penalty, patient_continuity_of_care).
narrative_ontology:constraint_vindicates(gendered_caregiving_penalty, separate_spheres_ideology).
narrative_ontology:constraint_vindicates(gendered_caregiving_penalty, ideal_worker_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN PHYSICIAN WITH YOUNG CHILDREN (SNARE) — Identity-locked rather than materially trapped: has medical degree, earning capacity, and structural mobility, but identity is constituted through both professional role (physician) and caregiving role (mother). Exit from medicine would require abandoning professional identity built over 11+ years of training. Staying requires either abandoning caregiving identity or accepting extraction through part-time penalties, promotion barriers, and burnout. The constraint extracts heavily because the agent cannot exit either identity frame without fundamental self-reconstitution. Theater ratio reflects performative accommodation policies (parental leave on paper, unusable in practice due to coverage gaps and career penalties).
constraint_indexing:constraint_classification(gendered_caregiving_penalty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMAN PHYSICIAN IN TRAINING (SNARE) — Constrained rather than identity-locked: facing specialty choice with full information about differential exit rates. Can choose lower-extraction specialties (dermatology, pathology) over higher-extraction ones (surgery, emergency medicine), but the constraint suppresses this choice through prestige hierarchies, debt burden requiring high-income specialties, and the fact that even 'family-friendly' specialties maintain ideal-worker norms. Extraction is high because the choice is between career penalty (lower-prestige specialty) and future exit (high-extraction specialty incompatible with caregiving). Suppression is high because debt, prestige, and institutional culture all push toward specialties that will later force exit.
constraint_indexing:constraint_classification(gendered_caregiving_penalty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE INSTITUTION (ROPE) — Experiences the constraint as coordination: the ideal-worker norm (unlimited availability, no caregiving responsibilities) solves the legitimate scheduling problem of 24/7 patient care coverage. Institutions benefit from the gendered exit pattern: women physicians exit at higher rates, reducing competition for leadership positions and allowing institutions to avoid costly accommodation infrastructure (on-site childcare, flexible scheduling, job-sharing). The institution has arbitrage-level exit: can recruit from the continuous pipeline of new graduates and does not bear the cost of training replacement physicians (medical schools and residency programs absorb that cost). Net beneficiary — extraction flows toward the institution through free disposal of trained physicians and avoidance of accommodation costs.
constraint_indexing:constraint_classification(gendered_caregiving_penalty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MALE PHYSICIAN (ROPE) — Experiences the constraint as coordination: the ideal-worker norm allows him to focus on career without caregiving penalty because unpaid domestic labor is performed by a partner or purchased. Mobile exit options: can move between institutions, specialties, or practice settings without caregiving constraints. Benefits from reduced competition (women exit at higher rates, opening leadership and high-income positions) and from the gendered division of labor that subsidizes his availability. The constraint coordinates his career advancement by removing competitors and by relying on gendered caregiving norms he does not challenge. Low effective extraction because he is a structural beneficiary with high mobility.
constraint_indexing:constraint_classification(gendered_caregiving_penalty, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PART-TIME WOMAN PHYSICIAN (TANGLED ROPE) — Constrained exit: has negotiated part-time arrangement, but faces promotion barriers, lower pay per hour worked, exclusion from leadership, and professional marginalization. Experiences both coordination (part-time work does solve the immediate caregiving-career conflict) and extraction (pays penalty in career trajectory, income, and professional status for accessing the accommodation). The constraint requires active enforcement: institutional norms treat part-time work as lack of commitment, and the penalty structure is maintained through formal policies (partnership track requirements, productivity metrics) and informal culture (availability signaling, face-time norms). Beneficiary (has caregiving time) and victim (pays career penalty) simultaneously — the definitional structure of tangled rope.
constraint_indexing:constraint_classification(gendered_caregiving_penalty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MEDICAL WORKFORCE EQUITY COALITION (SCAFFOLD) — Organized agents (medical associations' women-in-medicine sections, advocacy groups, policy researchers) see the constraint as temporary: demographic shift (women now 50%+ of medical school graduates) creates pressure for institutional change, and pilot programs (job-sharing, on-site childcare, flexible residency tracks) demonstrate feasible alternatives. Sunset logic: as women become majority of workforce, institutions that fail to accommodate will face recruitment and retention crises, forcing structural change. However, no formal sunset clause exists, and the coalition has constrained exit (cannot leave medicine, can only advocate within it). Moderate extraction because the coalition has agency and sees a path to change, but the path is generational and uncertain.
constraint_indexing:constraint_classification(gendered_caregiving_penalty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination function (24/7 patient care requires workforce availability) and asymmetric extraction (gendered caregiving norms concentrate costs on women physicians while institutions and male physicians benefit). The coordination function is real but could be met through alternative structures (shift work, job-sharing, team-based care). The extraction is substantial: 21.3% of women exit for childcare vs 4.2% of men, representing massive loss of trained capacity and individual career penalty. The constraint requires active enforcement through ideal-worker norms, promotion criteria, and cultural penalties for part-time work. Analytical classification: tangled rope, with the coordination story (patient care coverage) serving as partial cover for the extraction mechanism (gendered exit pattern that benefits institutions and male physicians).
constraint_indexing:constraint_classification(gendered_caregiving_penalty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_caregiving_penalty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gendered_caregiving_penalty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gendered_caregiving_penalty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_caregiving_penalty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_caregiving_penalty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts heavily from women physicians through three mechanisms: (1) forced exit from clinical practice (21.3% for childcare vs 4.2% for men), representing loss of 11+ years of training investment and future earning capacity; (2) career penalties for those who stay and use accommodation (part-time physicians face promotion barriers, lower hourly pay, leadership exclusion); (3) burnout and reduced career satisfaction for those who stay without accommodation (attempting to meet both ideal-worker and primary-caregiver norms simultaneously). The extraction flows to healthcare institutions (free disposal of trained physicians, avoidance of accommodation infrastructure costs) and to male physicians (reduced competition, subsidized availability through partners' unpaid labor). Extractiveness has increased over the interval from 0.52 (1970) to 0.68 (2020) as women's representation grew from ~10% to 50%+ of medical graduates, creating more victims while institutional structures remained unchanged. Suppression (0.72): High. Multiple mechanisms suppress exit and alternatives: (1) identity lock (professional identity built over 11+ years cannot be abandoned without self-reconstitution); (2) debt burden (median $200K+ requiring high-income specialty); (3) prestige hierarchies (family-friendly specialties are lower-status); (4) cultural penalties (part-time work treated as lack of commitment); (5) coverage requirements (institutions frame 24/7 availability as patient care necessity); (6) lack of alternative career paths (medical training is highly specialized). Suppression has increased over the interval from 0.58 (1970) to 0.72 (2020) as medical training lengthened (more sunk cost), debt increased, and ideal-worker norms intensified despite formal accommodation policies. Theater ratio (0.58): Moderate-high. Substantial gap between formal accommodation policies and actual usability. Institutions have proliferated policies (parental leave, part-time tracks, flexible scheduling) that appear to address the constraint but are rendered unusable by: (1) coverage gaps (no backup staffing for leave-takers); (2) career penalties (accommodation users face promotion barriers and income loss); (3) cultural norms (taking leave or working part-time signals lack of commitment); (4) productivity metrics (part-time physicians held to full-time productivity standards). The theater has increased over the interval from 0.35 (1970, when few formal policies existed) to 0.58 (2020, when policies are widespread but largely performative). The increase in theater ratio represents institutions' response to equity pressure: create appearance of accommodation without restructuring underlying work norms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon produces radically different classifications depending on the observer's position. Women physicians with young children experience pure extraction (snare): the constraint forces exit or severe penalty with no genuine coordination benefit to them. Healthcare institutions experience pure coordination (rope): the ideal-worker norm solves their scheduling problem and they benefit from the gendered exit pattern. Male physicians also experience coordination (rope): the constraint coordinates their career advancement by removing competitors and by relying on gendered caregiving norms they do not challenge. Part-time women physicians experience mixed coordination and extraction (tangled rope): the accommodation solves their immediate problem but extracts through career penalty. The equity coalition sees a temporary problem with a sunset (scaffold): demographic shift will force institutional change. The analytical observer sees tangled rope: genuine coordination function coexists with substantial asymmetric extraction. The perspectival gap is not a disagreement about facts but a structural consequence of position: beneficiaries see coordination, victims see extraction, and the analytical observer sees both. The gap reveals that 'coordination' and 'extraction' are not intrinsic properties of the constraint but indexical classifications that depend on the observer's structural relationship to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Women physicians with young children are primary victims with identity_locked exit, producing high directionality (d ≈ 0.85-0.95) and high effective extraction. The identity lock is cognitive rather than material: they have earning capacity and structural mobility, but exit from medicine requires abandoning professional identity built over 11+ years, while staying requires either abandoning caregiving identity or accepting severe extraction. The constraint extracts through forced choice between constitutive identities. Women physicians in training are secondary victims with constrained exit (d ≈ 0.70-0.80): can choose lower-extraction specialties but the choice is suppressed by debt, prestige, and culture. Healthcare institutions are primary beneficiaries with arbitrage exit (d ≈ 0.10-0.20, possibly negative): they benefit from ideal-worker norm through free disposal of trained physicians (medical schools absorb replacement training cost), avoidance of accommodation infrastructure, and reduced competition for leadership. Institutions have arbitrage-level exit because they can recruit continuously from the pipeline and do not bear training costs. Male physicians are secondary beneficiaries with mobile exit (d ≈ 0.25-0.35): benefit from reduced competition and from gendered division of labor that subsidizes their availability, but the benefit is less direct than institutions' benefit. Part-time women physicians are mixed (d ≈ 0.50-0.60): have negotiated accommodation (beneficiary) but pay career penalty (victim). The tangled-rope classification for this perspective derives from the simultaneous beneficiary/victim status. Medical workforce equity coalition is organized with constrained exit (d ≈ 0.45-0.55): has agency and sees path to change (reducing extraction) but cannot exit medicine and faces generational timeline (increasing extraction). Abstract victims (healthcare system capacity, patient continuity) have d ≈ 0.90-1.00 (full target, no exit, no advocate). The analytical observer computes tangled rope from the structural data: genuine coordination function (24/7 patient care coverage) coexists with substantial asymmetric extraction (gendered exit pattern benefits institutions and male physicians at women physicians' expense).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the coordination story (24/7 patient care requires physician availability) is both genuine and incomplete. The coordination function is real: patients do need continuous care, and physician availability does matter. But the coordination function could be met through alternative structures (shift work, team-based care, job-sharing) that do not concentrate costs on women physicians. The current structure is not the only way to solve the coordination problem; it is the way that also extracts from women physicians and benefits institutions and male physicians. The mandate (patient care coverage) has not outlived its function, but the specific implementation (ideal-worker norm + gendered caregiving division) extracts asymmetrically while claiming necessity. The analytical classification is tangled rope rather than snare because the coordination function is genuine, but the extraction is substantial and the asymmetry is not necessary to the coordination. The constraint is maintained through active enforcement (cultural penalties for accommodation, promotion barriers for part-time work, productivity metrics that assume no caregiving) rather than through the coordination function alone. The mandatrophy is resolved by recognizing that coordination and extraction coexist, and that the extraction is not a necessary cost of the coordination but a contingent feature of the current implementation that benefits specific actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_necessity_vs_social_construction,
    'Is the gendered caregiving penalty an inevitable consequence of biological reproductive differences, or a socially constructed constraint that could be eliminated through institutional redesign?',
    'Cross-national comparison: countries with robust parental leave, subsidized childcare, and flexible work norms (Nordic countries) vs countries without (US). If exit rate differential persists across all policy regimes, biological constraint. If differential narrows substantially with policy intervention, social construction.',
    'If biological: constraint is closer to mountain (immutable) and accommodation is the only option. If social construction: constraint is snare (extractive and eliminable) and institutional redesign is feasible. Current evidence suggests social construction (Nordic countries show much smaller gender gaps in medical workforce retention), but omega remains because US institutions frame the penalty as natural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_necessity_vs_social_construction, empirical, 'Whether caregiving penalty is biological necessity or social construction').

omega_variable(
    accommodation_vs_transformation,
    'Do accommodation policies (part-time tracks, parental leave) reduce extraction, or do they institutionalize a two-tier system that preserves the penalty while appearing to address it?',
    'Longitudinal career outcome analysis: compare women who use accommodation policies vs women who do not, controlling for specialty and cohort. If accommodation users reach leadership and income parity, policies reduce extraction. If accommodation users face persistent career penalties, policies are theatrical.',
    'If accommodation reduces extraction: scaffold perspective is correct, and incremental policy change is the path. If accommodation institutionalizes penalty: snare perspective is correct, and fundamental restructuring of work norms is required. Current evidence mixed: accommodation policies increase retention but users face promotion and income penalties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_vs_transformation, empirical, 'Whether accommodation policies reduce extraction or institutionalize two-tier system').

omega_variable(
    male_caregiving_uptake_threshold,
    'At what rate of male physician caregiving uptake does the institutional penalty structure collapse?',
    'Threshold analysis: model institutional response to increasing male parental leave usage. If 10% of male physicians took extended parental leave, would institutions maintain the career penalty, or would they redesign coverage systems? Historical precedent: when elite male workers adopt a practice (remote work, flexible hours), institutions accommodate rather than penalize.',
    'If threshold is low (10-20% male uptake): the penalty is not about caregiving per se but about gender, and male uptake would force institutional change. If threshold is high (40%+ male uptake): the penalty is about caregiving disruption to coverage, and gender is incidental. Current male uptake is ~4%, far below any plausible threshold, so the question remains unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(male_caregiving_uptake_threshold, conceptual, 'Male caregiving uptake rate required to force institutional accommodation').

omega_variable(
    patient_care_quality_vs_physician_availability,
    'Does the ideal-worker norm (unlimited physician availability) actually improve patient outcomes, or is it a coordination mechanism that serves institutional convenience at the expense of both physicians and patients?',
    'Outcome studies comparing patient care quality under different physician work models: traditional (long hours, high availability, high burnout) vs alternative (shift work, team-based care, work-hour limits). If traditional model shows better outcomes, the coordination function is genuine. If alternative models show equivalent or better outcomes, the ideal-worker norm is extractive theater.',
    'If traditional model is superior: the constraint has genuine coordination function and the extraction is a necessary cost. If alternative models are equivalent: the constraint is pure extraction disguised as patient care necessity, and the analytical tangled-rope classification should be reconsidered as snare. Current evidence suggests alternative models produce equivalent outcomes with lower physician burnout, but institutions resist adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_care_quality_vs_physician_availability, empirical, 'Whether ideal-worker norm improves patient outcomes or serves institutional convenience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_caregiving_penalty, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcp_theater_1970, gendered_caregiving_penalty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gcp_theater_1980, gendered_caregiving_penalty, theater_ratio, 10, 0.42).
narrative_ontology:measurement(gcp_theater_1990, gendered_caregiving_penalty, theater_ratio, 20, 0.48).
narrative_ontology:measurement(gcp_theater_2000, gendered_caregiving_penalty, theater_ratio, 30, 0.53).
narrative_ontology:measurement(gcp_theater_2010, gendered_caregiving_penalty, theater_ratio, 40, 0.57).
narrative_ontology:measurement(gcp_theater_2020, gendered_caregiving_penalty, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(gcp_extract_1970, gendered_caregiving_penalty, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gcp_extract_1980, gendered_caregiving_penalty, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gcp_extract_1990, gendered_caregiving_penalty, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(gcp_extract_2000, gendered_caregiving_penalty, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(gcp_extract_2010, gendered_caregiving_penalty, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(gcp_extract_2020, gendered_caregiving_penalty, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gcp_suppress_1970, gendered_caregiving_penalty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gcp_suppress_1990, gendered_caregiving_penalty, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(gcp_suppress_2010, gendered_caregiving_penalty, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(gcp_suppress_2020, gendered_caregiving_penalty, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_caregiving_penalty, resource_allocation).
narrative_ontology:affects_constraint(gendered_caregiving_penalty, medical_specialty_prestige_hierarchy).
narrative_ontology:affects_constraint(gendered_caregiving_penalty, physician_burnout_epidemic).
narrative_ontology:affects_constraint(gendered_caregiving_penalty, primary_care_workforce_shortage).

% DUAL FORMULATION NOTE:
% The gendered caregiving penalty is one constraint within a family of medical workforce constraints. It is downstream of the ideal-worker norm (a separate constraint about unlimited availability expectations) and upstream of specialty choice distortions (women avoiding high-extraction specialties) and workforce capacity loss (trained physicians exiting clinical practice). Each constraint in the family has its own extractiveness value reflecting its specific mechanism, but they form a mutually reinforcing network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_caregiving_penalty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
