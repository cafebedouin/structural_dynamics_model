% ============================================================================
% CONSTRAINT STORY: caregiving_duty_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caregiving_duty_extraction, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caregiving_duty_extraction
 *   human_readable: Caregiving Duty Extraction in Family and Institutional Contexts
 *   domain: interpersonal/institutional/labor
 *
 * SUMMARY:
 *   Caregiving duty extraction operates at the intersection of biological
 *   necessity, economic dependency, identity fusion, and institutional
 *   design. The constraint exhibits the full DR classification range:
 *   mountain (natural law view of inevitable caregiving), rope (genuine
 *   coordination of dependent care), tangled rope (mixed coordination and
 *   asymmetric labor extraction), snare (trapped caregiver with no exit),
 *   piton (institutional care system maintaining performative commitment
 *   while privatizing actual costs), and scaffold (organizing movements
 *   constructing alternative pathways). The constraint's core asymmetry is
 *   that caregiving work — essential to human reproduction and social
 *   continuity — is disproportionately assigned to specific family members
 *   (typically women) without compensation, while the coordination benefits
 *   (relational stability, distributed risk) accrue to the entire family
 *   system. The extractiveness measurement (0.58) reflects moderate-to-high
 *   accumulation: the primary caregiver systematically loses economic
 *   opportunity, professional development, and autonomous decision-making
 *   capacity, while the system and beneficiaries gain stability, cost
 *   savings, and unpaid labor. Suppression (0.68) is high because exit
 *   barriers are substantial and multidimensional: economic (income loss,
 *   housing insecurity), legal (custody risk, inheritance claims), social
 *   (family pressure, stigma), and psychological (identity loss, guilt). The
 *   theater ratio (0.55) reflects that caregiving is partially performative:
 *   family narratives about 'love,' 'duty,' and 'natural instinct' mask the
 *   economic extraction and normalize unpaid labor as morally rather than
 *   economically motivated.
 *
 * KEY AGENTS:
 *   - Primary Caregiver: Victim (powerless/trapped or moderate/identity_locked) — bears extraction of time, labor, identity, and economic mobility. Structurally mobile but identity-fused or materially dependent.
 *   - Care Recipient: Beneficiary (institutional/arbitrage) — genuinely benefits from coordination; experiences relational stability and continuous care access. Often unaware of extraction mechanism.
 *   - Co-Resident Non-Caregiver Family Members: Secondary beneficiary/secondary victim (institutional/constrained) — benefit from primary caregiver's labor while partially recognizing unequal distribution. Constrained by social norms from exiting the unequal arrangement.
 *   - Institutional Care System: Beneficiary (institutional/arbitrage) — outsources coordination costs to unpaid family labor; maintains arrangement through policy inertia and performative rhetoric about 'family values.'
 *   - Care Infrastructure Movements: Organized agents (organized/constrained) — build alternative pathways (paid leave, childcare subsidies, care worker unionization) with sunset logic for current extraction arrangement.
 *   - Analytical Observer: Risk of false summit — naturalization of 'caregiving is inevitable' into 'unpaid family duty extraction is inevitable.'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caregiving_duty_extraction, 0.58).
domain_priors:suppression_score(caregiving_duty_extraction, 0.68).
domain_priors:theater_ratio(caregiving_duty_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caregiving_duty_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(caregiving_duty_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(caregiving_duty_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caregiving_duty_extraction, tangled_rope).
narrative_ontology:human_readable(caregiving_duty_extraction, "Caregiving Duty Extraction in Family and Institutional Contexts").
narrative_ontology:topic_domain(caregiving_duty_extraction, "interpersonal/institutional/labor").

domain_priors:requires_active_enforcement(caregiving_duty_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caregiving_duty_extraction, primary_care_recipient).
narrative_ontology:constraint_beneficiary(caregiving_duty_extraction, institutional_employer).
narrative_ontology:constraint_beneficiary(caregiving_duty_extraction, male_family_members).
narrative_ontology:constraint_victim(caregiving_duty_extraction, primary_caregiver).
narrative_ontology:constraint_victim(caregiving_duty_extraction, caregiver_economic_mobility).
narrative_ontology:constraint_victim(caregiving_duty_extraction, caregiver_professional_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIMARY CAREGIVER (SNARE) — Structurally trapped through economic dependency, childcare interdependencies, and social isolation. Cannot exit care duties without material consequence (housing loss, child custody risk, income collapse). Experiences maximum extraction — the constraint extracts time, labor, identity, and economic opportunity with minimal reciprocal benefit. Exit barriers are material and insurmountable within biographical timescale.
constraint_indexing:constraint_classification(caregiving_duty_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PRIMARY CAREGIVER AS IDENTITY-LOCKED (TANGLED ROPE) — Structurally mobile (could theoretically exit through custody arrangements, institutional care, relocation) but functionally trapped by identity fusion. The caregiver's self-concept is constituted through the caregiving role — 'good mother/daughter,' 'the responsible one,' 'the heart of the family.' Exit would require abandoning this identity, becoming 'selfish' or 'abandoning duties.' The constraint coordinates genuine family stability AND asymmetrically extracts caregiver labor. Perceives the constraint as rope-like at biographical timescale (changeable if identity shifts) rather than immutable mountain. The perspectival gap between identity_locked and trapped perspectives reveals the identity-fusion mechanism.
constraint_indexing:constraint_classification(caregiving_duty_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: CARE RECIPIENT / INSTITUTIONAL CARE SYSTEM (ROPE) — Genuinely benefits from coordination: receives care, experiences relational stability, accesses family knowledge network. From this perspective the constraint is pure coordination — solving the problem of who ensures the dependent person's welfare. No extraction is experienced; the constraint is a coordination success. Zero-sum framing disappears. Institutional arbitrage: alternative care arrangements (professional caregiving, institutional facilities, paid home health aides) are available but costlier, so the constraint offers efficiency.
constraint_indexing:constraint_classification(caregiving_duty_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CO-RESIDENT NON-CAREGIVER FAMILY MEMBER (TANGLED ROPE) — Typically a spouse or adult sibling who benefits from the primary caregiver's unpaid labor while bearing only partial cost (household stability, childcare provision, elder care). Constrained by social norms ('family responsibility,' 'she's better at it,' 'he's the breadwinner') that justify not sharing care load equally. Coordinates family stability while extracting labor asymmetrically. Sees some constraint as necessary coordination but also recognizes unequal distribution. May recognize extraction while participating in it due to exit costs (family conflict, identity as 'good family member' who doesn't protest inequitable divisions).
constraint_indexing:constraint_classification(caregiving_duty_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL CARE SYSTEM (PITON) — Professional elder care, childcare systems, and disability services have largely outsourced their coordination function to unpaid family caregivers. The system maintains theatrical compliance with 'family values' and 'community care' narratives while the actual coordination burden falls unpaid onto individuals. The institutional system benefits from privatized care (lower costs, no fiscal responsibility) and maintains this arrangement through theater — media narratives about 'sandwich generation' and 'family values,' policy inertia that treats unpaid care as outside the economy. The system sees itself as degraded: policy actors recognize that care infrastructure is inadequate but maintain the status quo through performative gesture rather than structural change. Sunset clause is absent — no exit path from this arrangement is being constructed.
constraint_indexing:constraint_classification(caregiving_duty_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CARE INFRASTRUCTURE MOVEMENTS (SCAFFOLD) — Organized care worker unions, caregiver advocacy coalitions, and care infrastructure policy initiatives are building alternative coordination pathways: paid family leave, childcare subsidies, caregiver tax credits, care worker unionization, and shift from medical model to social model care. These movements have agency and are constructing a sunset clause — the transition from unpaid family duty extraction to professionalized, compensated care coordination. Constrained by institutional resistance and budget politics, but see a real exit path. Theater is declining (care strikes make extraction visible) as movements reframe care from 'duty' to 'work requiring compensation.'
constraint_indexing:constraint_classification(caregiving_duty_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL NECESSITY (MOUNTAIN) — From a civilizational/universal perspective, caregiving is inherent to human reproduction and child-rearing — it is a natural requirement, not a contingent institutional arrangement. Infants require care; frail elders require care; this is an immutable biological law. However, the structural data contradicts this mountain classification — the engine will compute this as a false summit. The biological necessity is real, but the *assignment of unpaid caregiving duty to specific family members* is contingent, not natural. The false summit reveals that 'caregiving is work' differs structurally from 'caregiving must be unpaid family duty.' One is mountain; the other is tangled rope or snare depending on context.
constraint_indexing:constraint_classification(caregiving_duty_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caregiving_duty_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(caregiving_duty_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(caregiving_duty_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(caregiving_duty_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(caregiving_duty_extraction, TR),
    TR >= 0.70.

:- end_tests(caregiving_duty_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Caregiving duty extraction is moderate-to-high. The primary caregiver systematically loses: (1) Economic opportunity — foregone wages, retirement savings, professional advancement during caregiving years; (2) Human capital — skills atrophy, credential maintenance lapse, network decay; (3) Decision autonomy — care-recipient needs override caregiver preferences in scheduling, location, major decisions; (4) Identity portfolio — self-concept narrows to caregiver role, other identities (professional, creative, social) atrophy. The measurement captures the cumulative extraction across these dimensions. Suppression (0.68): Multiple barriers prevent exit. Material barriers: economic dependency (caregiving produces no independent income), childcare/elder care costs (institutional alternatives are expensive), housing insecurity (family housing tied to caregiving role). Legal barriers: custody disputes, inheritance claims, spousal economic control. Social barriers: family pressure ('you can't abandon them'), community judgment, isolation from alternative reference groups. Psychological barriers: guilt, identity loss, internalized belief that 'I'm the only one who can do this.' Theater ratio (0.55): Moderate. Caregiving contains genuine coordination (ensuring dependent welfare, maintaining family stability) but is substantially framed through moral/emotional language rather than economic language. Narratives like 'it's just love,' 'family comes first,' 'she's naturally better at it' perform moral justification for what is structurally an economic extraction. The performative component has increased over the measurement interval as care work has become more intensive (longer lifespans, more complex medical care, more childcare hours) while remaining unpaid.
 *
 * PERSPECTIVAL GAP:
 *   The identity_locked perspective versus trapped perspective reveals the role of cognitive capture in maintaining extraction. Both perspectives describe similar material conditions: economic dependency, childcare costs, social pressure. But the trapped agent perceives these barriers as external and immovable ('I cannot leave because of objective conditions'). The identity_locked agent has structural mobility but cannot exercise it because their identity is constituted through the constraint ('I cannot leave because I am the caregiver; to leave would be to stop being me'). This is not a difference in material conditions but a difference in how the agent's self-concept relates to the constraint. The classification difference at biographical time — trapped → mountain, identity_locked → rope — reveals the mechanism: a trapped agent perceives the constraint as unchangeable regardless of perspective shift; an identity_locked agent perceives it as changeable IF the identity frame breaks. In practice, identity breaking is exceptionally difficult and requires sustained support (therapy, peer community, alternative role models, economic security to weather the transition). But structurally, identity_locked agents have a theoretical exit path that trapped agents do not. The gap between these two perspectives in the same biographical context is diagnostic: it shows the constraint's power is cognitive, not purely material.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across agent positions. Primary caregiver (victim + trapped/identity_locked): d ranges from 0.95 (trapped, powerless) to 0.89 (identity_locked, moderate). They bear maximum costs (time, opportunity, identity) with minimal compensation. Care recipient (beneficiary + arbitrage): d ≈ 0.05-0.15. They benefit substantially (continuous care, relational access) with no cost. Co-resident non-caregiver (mixed beneficiary + constrained): d ≈ 0.40-0.50. They benefit from primary caregiver's labor but also pay costs (reduced primary caregiver availability, family tension, cognitive dissonance about inequality). The sigmoid function f(d) applies: high d (victims) produces high f(d) ≈ 1.28-1.42 (amplified extraction experience); low d (beneficiaries) produces low/negative f(d) ≈ -0.12 to 0.02 (subsidized or neutral experience). Scope modifier σ(S): local (0.8) dampens, national (1.0) neutral, global (1.2) amplifies. A caregiver experiencing local constraint sees χ = 0.58 × 1.28 × 0.8 ≈ 0.60 (high extracted cost). An institutional system operating nationally sees χ = 0.58 × (-0.12) × 1.0 ≈ -0.07 (constraint subsidizes them). The directionality chain captures why the same constraint is experienced completely differently by different agents: the mathematical structure of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The caregiving duty extraction constraint demonstrates why mandatrophy resolution (preventing conflation of coordination with extraction) is essential. The naive analysis would classify this as pure Rope: caregiving genuinely coordinates dependent care and family stability; the coordination function is real. But the structural data reveals asymmetric extraction that pure Rope cannot accommodate: the caregiver bears costs the beneficiary does not, suppression is high, and alternative arrangements (institutional care, shared duties, paid care work) are suppressed by institutional design. Tangled Rope is the correct classification: genuine coordination function (family stability, dependent care) coexists with asymmetric extraction (unpaid labor, opportunity loss, identity narrowing). The mandatrophy is resolved by requiring that Tangled Rope declarations include beneficiaries, victims, AND enforcement mechanisms. This constraint has all three: beneficiaries (care recipient, institutional system, non-caregiver family members), victims (primary caregiver, caregiver opportunity cost, caregiver professional identity), and enforcement mechanisms (social norms, economic dependency, identity capture, institutional policy inertia). Without mandatrophy discipline, this would be misclassified as pure coordination (Rope), naturalizing the extraction as necessary to the coordination function. The constraint story framework forces disaggregation: the coordination function (genuine, valuable) is structurally separable from the extraction mechanism (contingent, redistributable). This separation is essential because it implies solution pathways: paid family leave, shared caregiving norms, professionalized care work, and institutional care alternatives can preserve coordination while removing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'What proportion of the measured suppression (0.68) is structural (economic, legal, material barriers) vs internalized (cognitive, identity-based entrapment)?',
    'Post-exit trajectory analysis: measure caregiver wellbeing and suppression markers for caregivers who have successfully exited; compare to those still trapped. If suppression persists after structural barriers are removed, component is internalized. If suppression drops sharply, component was primarily structural.',
    'If mostly structural: constraint can be loosened by improving material exit options (childcare access, income support, housing security). If mostly internalized: structural changes alone are insufficient — identity-reframing support (therapy, peer groups, narrative reframing) is required for exit to be exercised. If mixed: both interventions are necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Proportion of suppression that is structural vs internalized').

omega_variable(
    cyclical_vs_permanent_extraction,
    'Is the extraction mechanism cyclical (intermittent reinforcement: periods of crisis/validation alternating with calm/invisibility) or permanent (constant accumulation)?',
    'Longitudinal measurement of caregiver distress, conflict frequency, and validation experiences across at least 2-3 annual cycles; correlation analysis between cycle phase and extraction visibility. Identify reinforcement events (illness crisis requiring intensive care, expressions of gratitude/recognition, acute dependency) and calm periods (health stability, neglect of caregiver needs).',
    'If cyclical: intermittent reinforcement schedule explains continued participation despite extraction (variable ratio reward — most powerful conditioning mechanism). If permanent: rational cost-benefit analysis should produce faster exit, but doesn''t — suggests higher cognitive capture. Cyclical patterns indicate need for cycle-breaking interventions; permanent patterns indicate need for structural intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyclical_vs_permanent_extraction, empirical, 'Whether extraction follows cyclical or permanent pattern').

omega_variable(
    identity_fusion_mechanism,
    'What specific identity-fusion pathway binds the primary caregiver: professional identity (as primary source of competence/status), relational identity (self-worth constituted through role in relationships), or ideological identity (internalized gender/family norms)?',
    'Structured interviews or caregiver narratives analyzing how the caregiver describes themselves, their values, and what they would lose by exiting. Coding: frequency of identity-based language (''I''m the only one who can do this,'' ''this is who I am,'' ''what would my family think of me''), relational language (''they need me,'' ''I''m responsible for their wellbeing''), ideological language (''family comes first,'' ''women naturally care better''). Identify dominant pathway.',
    'Professional identity fusion: intervention targets alternative status sources and credential recognition. Relational identity fusion: intervention targets relational identity flexibility and new role models. Ideological identity fusion: intervention targets narrative reframing and peer group support. Different identity pathways require different exit support mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, conceptual, 'Primary identity-fusion mechanism binding caregiver').

omega_variable(
    care_recipient_agency_and_pressure,
    'To what degree is the care recipient a passive beneficiary vs active enforcer of the caregiver duty extraction? Does the recipient pressure or manipulate the caregiver to maintain the arrangement?',
    'Analysis of dyadic communication patterns (who initiates requests, tone, expression of gratitude or pressure); caregiver reports of manipulation (guilt induction, threats of abandonment, love withdrawal); care recipient statements about alternatives and acceptance of institutional care; observation of whether care recipient cooperates with or sabotages attempts at alternative arrangements.',
    'If passive beneficiary: extraction is structural-systemic, not relational-manipulative. If active enforcer: constraint has interpersonal abuse dynamics overlaid. If mixture: distinction between caregiver''s identity lock (their choice) vs care recipient''s pressure (limiting their choice). Affects intervention strategy and whether to address dyadic dynamics or structural alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_recipient_agency_and_pressure, empirical, 'Whether care recipient is passive beneficiary or active enforcer').

omega_variable(
    male_caregiver_participation,
    'Why do men perform substantially less unpaid caregiving despite equivalent structural opportunity? Is the barrier institutional norm (identity-locked to gender role), economic incentive (labor market premium for male non-caregivers), power dynamics (delegation to female family members), or preference?',
    'Comparative analysis of household time use data by gender and bargaining position; analysis of male caregivers (minority) to identify what structural conditions enable participation; analysis of caregiver narratives about gender division; economic analysis of career penalty by caregiver gender.',
    'If norm-based: intervention targets identity reframing and model-shifting. If incentive-based: intervention targets economic structure (eliminate caregiver career penalty, equalize labor market returns). If power-based: intervention targets domestic bargaining dynamics. If preference-based: suggests constraint is coordination rather than extraction from male perspective. Different mechanisms point to different lever points for change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(male_caregiver_participation, empirical, 'Explanation for gendered caregiving distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caregiving_duty_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(care_tr_t0, caregiving_duty_extraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(care_tr_t5, caregiving_duty_extraction, theater_ratio, 5, 0.5).
narrative_ontology:measurement(care_tr_t10, caregiving_duty_extraction, theater_ratio, 10, 0.55).
narrative_ontology:measurement(care_tr_t3, caregiving_duty_extraction, theater_ratio, 3, 0.46).
narrative_ontology:measurement(care_tr_t7, caregiving_duty_extraction, theater_ratio, 7, 0.53).

% Extraction over time
narrative_ontology:measurement(care_be_t0, caregiving_duty_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(care_be_t5, caregiving_duty_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(care_be_t10, caregiving_duty_extraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(care_be_t2, caregiving_duty_extraction, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(care_be_t8, caregiving_duty_extraction, base_extractiveness, 8, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caregiving_duty_extraction, attachment_coordination).
narrative_ontology:boltzmann_floor_override(caregiving_duty_extraction, 0.12).
narrative_ontology:affects_constraint(caregiving_duty_extraction, gender_wage_gap).
narrative_ontology:affects_constraint(caregiving_duty_extraction, labor_market_motherhood_penalty).
narrative_ontology:affects_constraint(caregiving_duty_extraction, institutional_care_system_privatization).
narrative_ontology:affects_constraint(caregiving_duty_extraction, elder_care_infrastructure_deficit).

% DUAL FORMULATION NOTE:
% Caregiving duty extraction decomposes into multiple structurally distinct constraints sharing common agents but with different ε values: (1) attachment_coordination (ε≈0.25) — emotional bonding and relational stability, low extraction; (2) care labor extraction (ε≈0.58) — allocation of unpaid care work, high extraction; (3) institutional privatization (ε≈0.45) — cost displacement to family from public care systems, moderate extraction. The story represents the labor extraction constraint; link to sibling stories for full constraint family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caregiving_duty_extraction, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
