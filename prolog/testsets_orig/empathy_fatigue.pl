% ============================================================================
% CONSTRAINT STORY: empathy_fatigue
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_empathy_fatigue, []).

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
 *   constraint_id: empathy_fatigue
 *   human_readable: Empathy Fatigue: Structural Depletion of Emotional Labor Capacity
 *   domain: social/psychological/interpersonal
 *
 * SUMMARY:
 *   Empathy fatigue emerges as a structural constraint when demand for
 *   emotional labor exceeds sustainable supply, creating an extraction
 *   mechanism where care workers transfer their depletion costs to themselves
 *   (through guilt and identity crisis) or to care recipients (through
 *   withdrawn empathy). The constraint operates across multiple institutional
 *   domains — healthcare, social work, therapy, education, activism — but the
 *   underlying structure is consistent: individuals are expected to absorb
 *   others' suffering without institutional recognition that empathy is a
 *   finite resource requiring recovery. Empathy fatigue demonstrates the full
 *   range of DR classification because different actors experience the same
 *   phenomenon from fundamentally different structural positions. Care
 *   workers see extraction and entrapment. Institutions see coordination
 *   (deploying emotional labor efficiently). High-demand populations see
 *   insurance against neglect. Advocacy movements see a temporary problem
 *   solvable through systemic redesign. Self-care ideology sees individual
 *   pathology. The psychological natural law perspective risks naturalizing
 *   what is actually a contingent institutional choice to externalize the
 *   cost of empathy work onto the practitioners themselves.
 *
 * KEY AGENTS:
 *   - Frontline Emotional Laborers: Primary victims (powerless/trapped) — healthcare workers, therapists, social workers, teachers, crisis counselors who absorb others' trauma and suffering. Trapped by economic dependency and professional identity; cannot exit without material hardship and identity dissolution.
 *   - High-Demand Populations: Primary beneficiaries and secondary victims (powerless/trapped) — chronically ill, traumatized, disabled, grieving individuals who require sustained empathic engagement. Structurally dependent on human empathy; face deteriorating care quality as providers deplete.
 *   - Healthcare and Social Institutions: Beneficiaries (institutional/arbitrage) — extract labor value from practitioners without compensating for emotional depletion. Arbitrage available (outsource, reduce services, shift burden to lower-paid workers).
 *   - Mental Health Advocacy Movements: Organized agents (organized/constrained) — burnout researchers, union organizers, wellness researchers building alternative structures (mandatory rest, peer support, technology-assisted assessment, rotational assignments).
 *   - Self-Care Industry: Institutional actor maintaining piton (institutional/arbitrage) — wellness companies, meditation apps, coaching programs promote individual responsibility framing rather than systemic change.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional empathy extraction as psychological necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(empathy_fatigue, 0.58).
domain_priors:suppression_score(empathy_fatigue, 0.62).
domain_priors:theater_ratio(empathy_fatigue, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(empathy_fatigue, extractiveness, 0.58).
narrative_ontology:constraint_metric(empathy_fatigue, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(empathy_fatigue, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(empathy_fatigue, snare).
narrative_ontology:human_readable(empathy_fatigue, "Empathy Fatigue: Structural Depletion of Emotional Labor Capacity").
narrative_ontology:topic_domain(empathy_fatigue, "social/psychological/interpersonal").

domain_priors:requires_active_enforcement(empathy_fatigue).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(empathy_fatigue, high_demand_populations).
narrative_ontology:constraint_beneficiary(empathy_fatigue, institutional_care_systems).
narrative_ontology:constraint_victim(empathy_fatigue, frontline_emotional_laborers).
narrative_ontology:constraint_victim(empathy_fatigue, care_workers).
narrative_ontology:constraint_victim(empathy_fatigue, helpers_and_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED CARE WORKER (SNARE) — Structurally trapped by dual dependence: economic (employment, income, healthcare benefits) and identity (professional self-concept as caregiver). Cannot exit without material hardship and identity dissolution. Faces full extraction: emotional depletion without compensation, institutional blame for reduced empathy, no structural support for recovery. Maximum suppression — alternatives are presented as moral failure ('not caring enough').
constraint_indexing:constraint_classification(empathy_fatigue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMPASSION-FATIGUED THERAPIST (TANGLED ROPE) — Constrained by licensing requirements, client relationships, and reputation. Faces extraction through unlimited emotional demand, but also genuine coordination: therapeutic relationships inherently require sustained empathy, and burnout undermines the therapeutic function itself. Mixed: derives professional identity and income from empathy work, but also bears the cost of depletion.
constraint_indexing:constraint_classification(empathy_fatigue, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE INSTITUTION (ROPE) — Experiences empathy fatigue constraint as a coordination mechanism: deploying empathic labor efficiently solves the problem of maintaining quality care with limited resources. Arbitrage available (shift burden to private care, reduce services, outsource to lower-cost markets). Net beneficiary — institution captures surplus labor value during depletion cycle.
constraint_indexing:constraint_classification(empathy_fatigue, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-DEMAND POPULATIONS (SNARE) — Individuals with chronic illness, trauma, disability, or grief require sustained empathic engagement. Trapped by structural need (no alternative to human empathy; algorithmic substitutes fail). Extraction flows toward these populations initially, but as care workers deplete, care workers withdraw empathy — the supply-side collapse eventually reaches the demand side as deteriorating care quality. Double trap: cannot exit need, cannot exit the consequence of caregiver depletion.
constraint_indexing:constraint_classification(empathy_fatigue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: MENTAL HEALTH ADVOCACY MOVEMENTS (SCAFFOLD) — Organized agents (burnout researchers, union organizers, policy advocates, wellness researchers) are building structural alternatives: mandatory rest periods, peer support systems, rotational assignments, technology-assisted assessment to reduce unnecessary empathy drain. These are temporary supports with sunsets — the goal is to transform empathy fatigue from individual pathology ('I'm not empathic enough') to systemic recognition ('the system demands unsustainable empathy'). As norms shift, the individual blame mechanism loses force.
constraint_indexing:constraint_classification(empathy_fatigue, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PROFESSIONAL SELF-CARE IDEOLOGY (PITON) — The therapeutic response to empathy fatigue ('practice self-care,' 'maintain boundaries,' 'recharge your emotional batteries') is substantially performative. Self-care rituals (meditation apps, wellness retreats, breathing exercises) are effective for mild fatigue but insufficient for systemic depletion. The ideology persists through institutional inertia — it shifts responsibility to individuals, reduces institutional liability, and creates markets for self-care products. Theater ratio reflects that the primary function (recognizing systemic empathy extraction) has atrophied, replaced by theatrical individual interventions.
constraint_indexing:constraint_classification(empathy_fatigue, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PSYCHOLOGICAL NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, empathy fatigue appears as an immutable property of human psychology: finite emotional resources, cognitive limits on attention to suffering, and inherent depletion from absorbing others' trauma are natural laws, not extractive mechanisms. However, this classification is a false summit — cross-cultural data, historical variation in empathy fatigue rates, and institutional design effects all demonstrate that much of what appears 'natural' is contingent on how care work is structured and compensated.
constraint_indexing:constraint_classification(empathy_fatigue, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(empathy_fatigue_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(empathy_fatigue, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(empathy_fatigue, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(empathy_fatigue, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(empathy_fatigue, TR),
    TR >= 0.70.

:- end_tests(empathy_fatigue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Care workers experience systematic extraction of emotional labor: (1) demand vastly exceeds sustainable supply (patient/client load ratios, crisis caseloads), (2) institutional blame frames depletion as individual failure ('not emotionally resilient enough'), (3) depletion leads to withdrawal of empathy toward future clients, creating a negative feedback loop, (4) practitioners bear the entire cost while institutions extract the labor benefit. The value reflects that extraction is real and measurable but not total — some care settings maintain reasonable demand, some practitioners find meaning that sustains motivation, and advocacy movements are creating alternatives. Suppression (0.62): Moderate-high. Significant barriers to exit include: economic dependency (few alternative careers with equivalent income, healthcare coverage tied to employment), identity fusion (professional identity constituted through caregiving), institutional blame (portrayed as personal weakness or moral failure), and cultural narratives normalizing caregiver sacrifice ('it's a calling'). Suppression is not total — some practitioners do exit, and awareness of burnout is increasing. Theater ratio (0.48): Moderate. Self-care interventions (meditation, boundary-setting, wellness retreats) are partially performative — they address symptoms without addressing the structural problem (unsustainable demand ratios, lack of institutional support for recovery). However, they are not purely theatrical; genuine relief and meaning are possible through individual coping. The theater increases over the measurement interval as self-care ideology becomes more prominent and systemic intervention remains absent.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Frontline workers see pure extraction and entrapment (Snare) — they experience the constraint as inexorable demand with no exit and guilt-based blame. Institutions see coordination (Rope) — deploying emotional labor efficiently solves the care provision problem. High-demand populations initially see insurance (benefit from empathy) but experience the extraction cascade (care quality collapses as providers deplete). Advocacy movements see a solvable problem (Scaffold) — systemic redesign can distribute empathy demand across teams, introduce technology assistance, and create mandatory recovery. The self-care ideology sees individual pathology (Piton) — the constraint is portrayed as personal weakness rather than systemic design. The civilizational analytical observer risks seeing psychology (Mountain) — finite empathy is a law of nature — but this naturalizes what is actually institutional design: empathy fatigue is not inherent to caregiving; it is a consequence of demand-to-supply ratios, lack of institutional support, and blame-shifting that transforms depletion into moral failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from each agent's structural position — their power, exit options, and relationship to extraction flow. Frontline workers classified as powerless/trapped produce high d (→ high f(d) → high χ): they are full targets bearing maximal extraction. Institutions classified as institutional/arbitrage produce low d (→ low/negative f(d) → they experience negative χ): they are net beneficiaries. High-demand populations classified as powerless/trapped occupy a paradoxical position: they are primary victims (dependent on empathy) but also beneficiaries (capture the output of emotional labor). The directionality recognizes this by treating them as trapped agents who initially benefit but ultimately suffer as care worker depletion leads to care quality collapse. Advocacy movements classified as organized/constrained produce moderate d: they have agency and some exit paths (organizing, policy work) but face institutional resistance. The scaffold classification reflects that their time horizon is long enough to build alternatives (generational), reducing experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Empathy fatigue resolves mandatrophy by revealing that classification depends entirely on perspective. The question is not 'is this extraction or coordination?' but 'who is the analysis for?' From the care worker's position, it is Snare (extraction + entrapment). From the institution's position, it is Rope (coordination + benefit). The analytical observer's risk is naturalizing this extraction as inevitable psychology rather than contingent institutional design. The mandatrophy is resolved by the scaffold perspective: organized actors are building alternatives that would shift classification from Snare/Rope to Scaffold/Rope across multiple perspectives simultaneously. If these alternatives succeed, the extraction mechanism (institutional blame, unsustainable demand ratios) loses force, and the constraint reclassifies as a temporary coordination problem with a sunset. The fact that different types coexist is not contradictory — it reflects that the constraint structure genuinely contains coordination (care provision), extraction (caregiver depletion without compensation), and possibilities for systemic redesign (advocacy movements).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_individual_depletion,
    'Is empathy fatigue primarily a structural phenomenon (unsustainable demand ratios, inadequate rest, institutional blame) or an individual psychological limit?',
    'Cross-institutional comparison: empathy fatigue rates in systems with mandatory rest vs. open-ended shift systems; rates in settings with institutional support vs. isolated practice; longitudinal tracking of individual practitioners across different institutional contexts',
    'If structural: classification remains Snare/Tangled Rope, beneficiaries are institutional actors, intervention targets are systemic design. If individual: classification downgrades toward Rope/Scaffold, beneficiaries are care recipients who benefit from emotional labor, intervention targets are individual resilience.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_individual_depletion, empirical, 'Whether empathy fatigue is structural demand design or individual psychological limit').

omega_variable(
    empathy_substitution_feasibility,
    'Can algorithmic systems (AI therapists, automated assessment, predictive intervention) sustainably substitute for human empathic engagement, or do they fail at critical touch points?',
    'Comparative outcomes: AI-assisted vs. human-only care for chronic conditions; user satisfaction and clinical efficacy; longitudinal tracking of individuals who transition from human to algorithmic support; analysis of failure modes where algorithm substitution breaks down',
    'If feasible substitution: empathy fatigue becomes a transition problem (Scaffold type) solvable by technology adoption. If substitution fails: empathy fatigue is structurally inevitable (Snare type) and must be managed through demand reduction or extraction acknowledgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empathy_substitution_feasibility, empirical, 'Whether algorithmic systems can substitute for human empathic engagement').

omega_variable(
    identity_lock_vs_economic_trap,
    'Are care workers trapped primarily by economic dependency (alternative income sources unavailable) or by identity fusion (professional self-concept constituted through caregiving)?',
    'Analysis of exit decisions: do care workers leave when alternative employment becomes available? Longitudinal tracking of leavers vs. stayers controlling for income alternatives; interview data on departure rationales; comparison of exit rates across jurisdictions with different income supports',
    'If economic trap: exit barriers are material and could be reduced by income support, retraining, or policy intervention. If identity lock: exit barriers are cognitive, and even with financial support, practitioners remain in depleting roles because they cannot imagine themselves outside caregiving identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_economic_trap, empirical, 'Whether care worker entrapment is economic or identity-based').

omega_variable(
    beneficiary_awareness_and_complicity,
    'Do high-demand populations (chronically ill, traumatized, disabled individuals) actively demand empathy beyond what is sustainable, or do they receive empathy as a passive institutional good?',
    'Survey data on demand expectations vs. received empathy; analysis of patient/client feedback distinguishing ''I need more empathy'' from ''I am satisfied with care quality''; ethnographic study of care interactions; comparison of empathy demand across contexts with different institutional norms',
    'If active demand exceeds supply: beneficiaries are partially complicit in driving care worker depletion, and intervention must address demand side. If passive receipt: beneficiaries are victims of both initial deprivation (receiving insufficient empathy as systems collapse) and extraction (bearing consequences of caregiver depletion). Classification and intervention differ substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_awareness_and_complicity, conceptual, 'Whether high-demand populations actively drive unsustainable empathy expectations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(empathy_fatigue, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empfat_tr_t0, empathy_fatigue, theater_ratio, 0, 0.3).
narrative_ontology:measurement(empfat_tr_t3, empathy_fatigue, theater_ratio, 3, 0.38).
narrative_ontology:measurement(empfat_tr_t6, empathy_fatigue, theater_ratio, 6, 0.45).
narrative_ontology:measurement(empfat_tr_t10, empathy_fatigue, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(empfat_be_t0, empathy_fatigue, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(empfat_be_t3, empathy_fatigue, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(empfat_be_t6, empathy_fatigue, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(empfat_be_t10, empathy_fatigue, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(empathy_fatigue, attachment_coordination).
narrative_ontology:affects_constraint(empathy_fatigue, healthcare_labor_shortage).
narrative_ontology:affects_constraint(empathy_fatigue, therapist_burnout_crisis).
narrative_ontology:affects_constraint(empathy_fatigue, compassion_satisfaction_decline).

% DUAL FORMULATION NOTE:
% Empathy fatigue is downstream of multiple structural constraints: inadequate care worker compensation (constraint on economic sustainability), institutional failure to limit patient/client load (constraint on care demand), and cultural expectation that caregiving is a moral calling rather than labor (constraint on emotional labor boundaries). Each upstream constraint contributes to the empathy fatigue downstream. The network reflects causal interdependency: if any upstream constraint were resolved, empathy fatigue classification would shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(empathy_fatigue, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
