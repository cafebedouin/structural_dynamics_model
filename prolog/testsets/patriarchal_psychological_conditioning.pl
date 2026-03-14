% ============================================================================
% CONSTRAINT STORY: patriarchal_psychological_conditioning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patriarchal_psychological_conditioning, []).

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
 *   constraint_id: patriarchal_psychological_conditioning
 *   human_readable: Patriarchal Psychological Conditioning
 *   domain: social/interpersonal/identity
 *
 * SUMMARY:
 *   Patriarchal psychological conditioning is a constraint that operates
 *   simultaneously at interpersonal, institutional, and identity levels,
 *   creating structural extraction masked by naturalization narratives. The
 *   constraint involves the systematic training of subordinate agents
 *   (typically women, femme-coded individuals, and gender-nonconforming
 *   people) into psychological frames that naturalize their own
 *   subordination, extract labor and deference, suppress epistemic autonomy,
 *   and condition identity around submission or diminishment. Unlike naked
 *   coercion, psychological conditioning operates through internalization —
 *   the target becomes self-enforcing, naturalizing the constraints as
 *   identity-constitutive. The constraint exhibits all the hallmarks of a
 *   snare: high extractiveness (0.68), high suppression (0.72), minimal
 *   coordination function that could justify the extraction asymmetry, and
 *   reliance on suppressing alternatives (other identity frames, other
 *   relationship models, other social arrangements). The theater ratio (0.81)
 *   reflects that patriarchal institutions maintain the constraint through
 *   symbolic performance — ritual affirmation of gender hierarchies,
 *   narrative templates in media and education, ceremonial role performance
 *   in family and religious contexts — as much as through functional
 *   enforcement. The measurements show extractiveness increasing over the
 *   biographical interval (0.45→0.68) and theater ratio rising (0.65→0.81),
 *   indicating progressive entrenchment and increasing reliance on symbolic
 *   enforcement as material alternatives become available. This decomposition
 *   separates into three distinct constraint stories: (1)
 *   patriarchal_psychological_conditioning (this file) — the identity-lock
 *   and psychological extraction mechanism, ε=0.68; (2)
 *   patriarchal_institutional_enforcement (to be written) — legal, economic,
 *   and religious apparatus that sustains suppression, ε=0.55; (3)
 *   patriarchal_relational_coordination (to be written) — the genuine
 *   coordination functions (household management, childcare coordination,
 *   resource sharing) that patriarchal framing usurps for asymmetric
 *   extraction, ε=0.28.
 *
 * KEY AGENTS:
 *   - Conditioned Subject: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with subordinate role; experiences maximal psychological extraction
 *   - Structurally Constrained Subject: Co-victim (moderate/constrained) — faces material barriers to exit (economic dependency, violence threat, childcare responsibility); different exit profile than identity-locked but same snare classification
 *   - Patriarchal Authority Holder: Primary beneficiary (institutional/arbitrage) — captures labor, deference, epistemic authority; experiences constraint as coordinating mechanism; minimal exit cost
 *   - Partially Captured Authority Holder: Mixed actor (moderate/constrained) — conditioned into provider/authority role; experiences mixed coordination (family stability) and extraction (emotional suppression); identity-locked into role despite beneficiary position
 *   - Institutional Apparatus: Executor (institutional/arbitrage) — legal systems, religious institutions, educational frameworks, media production that enforce patriarchal conditioning through symbolic performance; sees own role as degraded (piton perspective)
 *   - Feminist Counter-Coalition: Organized resistance (organized/constrained) — builds alternative institutional structures, identity framings, and economic pathways; operates from scaffold perspective with sunset logic
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing contingent arrangements as immutable features of human nature or biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patriarchal_psychological_conditioning, 0.68).
domain_priors:suppression_score(patriarchal_psychological_conditioning, 0.72).
domain_priors:theater_ratio(patriarchal_psychological_conditioning, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patriarchal_psychological_conditioning, extractiveness, 0.68).
narrative_ontology:constraint_metric(patriarchal_psychological_conditioning, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(patriarchal_psychological_conditioning, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patriarchal_psychological_conditioning, snare).
narrative_ontology:human_readable(patriarchal_psychological_conditioning, "Patriarchal Psychological Conditioning").
narrative_ontology:topic_domain(patriarchal_psychological_conditioning, "social/interpersonal/identity").

domain_priors:requires_active_enforcement(patriarchal_psychological_conditioning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patriarchal_psychological_conditioning, patriarchal_authority_holders).
narrative_ontology:constraint_victim(patriarchal_psychological_conditioning, conditioned_subordinates).
narrative_ontology:constraint_victim(patriarchal_psychological_conditioning, epistemic_autonomy).
narrative_ontology:constraint_victim(patriarchal_psychological_conditioning, identity_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDITIONED SUBJECT (SNARE) — Structurally mobile (has legal capacity, potential economic means, access to information) but identity-locked through internalized patriarchal frameworks. Identity constituted through gendered role assignments, desirability metrics, submissive framing. Exit would require becoming a different person — abandoning internalized obligation narratives, self-blame conditioning, and fused relational identity. Experiences maximal extraction: labor, deference, epistemic suppression, bodily autonomy constraints, all naturalized as inherent to identity.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: STRUCTURALLY CONSTRAINED SUBJECT (SNARE) — Faces material barriers to exit: economic dependency, childcare responsibilities, housing insecurity, social ostracism, violence threat. Unlike identity-locked perspective, barriers are external and material, not cognitive. High suppression through coercive institutional entanglement (marriage law, family structure, labor market discrimination). Exit is possible but at devastating cost. Still experiences the constraint as snare — high extraction with minimal coordination function.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PATRIARCHAL AUTHORITY HOLDER (ROPE) — Beneficiary. Experiences the constraint as pure coordination: delegating childcare, emotional labor, household management, and sexual access through gendered role assignment. Receives labor extraction, authority legitimacy, and leisure time without corresponding obligation reciprocity. From this position, the constraint appears as coordinating mechanism that enables household/social function. Low experienced suppression because exit costs are minimal — can exit without identity dissolution or material devastation.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PARTIALLY CAPTURED AUTHORITY HOLDER (TANGLED ROPE) — Some patriarchal authority holders are themselves conditioned into the role, experiencing genuine coordination (stable family structure) alongside extraction (emotional labor suppression, vulnerability taboo, performance of invulnerability). Moderate extraction but real beneficiary status. Requires active enforcement of emotional suppression norms — the constraint demands constant reinforcement of detachment. Some exit capacity but identity-locked into provider/authority role, making exit psychologically costly without being materially catastrophic.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL APPARATUS (PITON) — Legal frameworks (marriage, property, guardianship), religious institutions (theological gender hierarchies), educational systems (curriculum that naturalizes gender roles), media production (narrative templates reinforcing gendered expectations). These systems maintain patriarchal conditioning through institutional inertia and theatrical performance of natural order. Theater ratio is high (0.81) because much enforcement is symbolic — ritual affirmation of hierarchy, ceremonial role performance — rather than functional coordination. The apparatus sees itself as degraded: institutional gatekeepers know the gender narratives are narratives, not laws, yet maintain them through lack of perceived alternatives.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FEMINIST COUNTER-COALITION (SCAFFOLD) — Organized resistance movements (feminist theory, consciousness-raising groups, economic independence advocacy, legal reform, education access) frame the constraint as temporary, solvable through institutional transformation. Low effective extraction from this perspective because the coalition has agency, identifies mechanisms of control, and builds alternative structures (economic autonomy, epistemic authority, identity self-determination). Suppression from the coalition is less than from conditioned subject perspective because members have consciously exited the identity-lock framework. Has sunset clause logic: as women's educational access, economic participation, and institutional representation increase, the extraction mechanism loses force.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From civilizational timescale, risks seeing gender hierarchies as immutable features of human social organization, grounded in biology, evolutionary fitness, or natural human complementarity. This perspective naturalizes contingent institutional arrangements as laws of nature. The engine will detect this as a false summit: base properties show extractiveness (0.68) and suppression (0.72) inconsistent with mountain thresholds (ε ≤ 0.25, suppression ≤ 0.05). The naturalization is the constraint's cover story, not its structure.
constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patriarchal_psychological_conditioning_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patriarchal_psychological_conditioning, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patriarchal_psychological_conditioning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patriarchal_psychological_conditioning, TR),
    TR >= 0.70.

:- end_tests(patriarchal_psychological_conditioning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts labor (household work, emotional labor, sexual access), epistemic authority (decision-making power, credibility, knowledge validation), identity autonomy (role assignment, desirability metrics, self-determination), and temporal resources (time allocation decisions made by authority holder). The 0.68 value reflects that some of these extractions are partially legitimated through reciprocity narratives ('I provide economically, you provide domestically') that lower perceived extraction below true asymmetry. Pure structural extraction (comparing labor hours, decision power, resource control, risk exposure, autonomy constraints) would be higher (~0.75+), but psychological legitimation narratives reduce experienced extraction. Suppression (0.72): High. Multiple reinforcement mechanisms maintain the lock: identity fusion (exit requires becoming a different person), material dependency (economic precarity without authority holder support), institutional entanglement (legal marital status, custody frameworks, reproductive control), violence threat (physical harm for non-compliance), social ostracism (loss of community, family), and epistemically (counter-narratives suppressed, alternative models unavailable). Theater ratio (0.81): High, increasing over interval. Patriarchal enforcement increasingly relies on symbolic/theatrical performance as material alternatives become available. Traditional enforcement (economic necessity, explicit legal disability) has declined in high-development contexts; symbolic enforcement (media narrative templates, gendered expectation reinforcement, aesthetic/desirability conditioning, ceremonial role performance) has intensified. This indicates theater substitution: as one enforcement mode weakens, theatrical maintenance intensifies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence across the six types. The conditioned subject (powerless/identity_locked) perceives snare: identity-locked into subordinate frame, extraction hidden through naturalization, no exit without becoming a different person. The structurally constrained subject (moderate/constrained) also perceives snare but with different binding mechanisms: material barriers (economic dependency, violence threat, childcare responsibility) rather than identity fusion. The authority holder (institutional/arbitrage) perceives rope: coordination mechanism efficiently delegating household/emotional labor, low exit cost for them, minimal experienced suppression. The partially captured authority holder perceives tangled rope: genuine coordination function (family stability) alongside extraction (emotional suppression, vulnerability taboo, identity role-lock). The institutional apparatus perceives its role as piton: theatrical maintenance of patriarchal frameworks, declining functional necessity, sustained through inertia. The organized feminist counter-coalition perceives scaffold: temporary institutional arrangement being systematically dismantled through legal reform, education access, economic independence, consciousness-raising; sunset clause operates as women's institutional participation increases. The civilizational analytical observer risks perceiving mountain: naturalizing contingent institutional arrangements as immutable features of human nature or evolutionary fitness. This range of perspectives from single constraint structure is diagnostic that no single type is 'correct' — the constraint is genuinely different experiences from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Conditioned subject: victim status + identity_locked exit = high d (~0.85-0.90) → high f(d) (~1.25-1.38) → experienced χ is high. Structurally constrained subject: victim status + constrained exit = high d (~0.75-0.85) → high f(d) (~1.15-1.25) → experienced χ is high. Authority holder: beneficiary status + arbitrage exit = low d (~0.15-0.25) → negative/low f(d) (~-0.01 to 0.15) → experienced χ is low or negative (perceives as beneficial). Partially captured authority holder: beneficiary status + constrained exit = moderate d (~0.40-0.50) → moderate f(d) (~0.40-0.65) → experienced χ is moderate. Institutional apparatus: beneficiary status + arbitrage exit = low d (~0.10-0.20) → negative f(d) (~-0.12 to 0.02) → experienced χ is low (perceives theater as sufficient). Feminist coalition: victim group (on behalf of constrained subjects) + organized/constrained exit = organized power modification → experienced χ is lower because coalition has agency and exit visibility. Analytical observer: neither beneficiary nor victim, analytical exit → canonical d (~0.73) → f(d) ~1.15 → neutral observational χ. The engine derives these automatically from declarations; the perspectival gap reflects true structural difference in exit options and beneficiary positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   Patriarchal conditioning resolves mandatrophy by demonstrating that 'the constraint' is not singular but rather a presheaf of constraint perspectives over the observation site. The conditioned subject's snare is not 'wrong' — it is her structural reality. The authority holder's rope is not 'wrong' — it is his structural reality. The institutional apparatus's piton is not 'wrong' — it reflects genuine degradation of patriarchal enforcement mechanisms. The feminist coalition's scaffold is not 'wrong' — it reflects real institutional change trajectories. The civilization observer's mountain is not 'wrong' in its classification mechanics, but it is false in its ontological claim (naturalizing contingency). The mandatrophy resolves by accepting that all six types are perspectival truths and none is the transcendent answer. What matters is: (1) recognizing which perspective you are measuring from, (2) understanding that the authority holder's rope is the conditioned subject's snare, (3) identifying that the 'natural law' view is covering up institutional extraction, (4) recognizing that institutional change alone (scaffold) may not dissolve the identity-lock component, and (5) understanding that the constraint family requires multiple stories with different ε values for material enforcement (higher ε) and relational coordination (lower ε).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_material_constraint_ambiguity,
    'To what extent is the observed suppression structural (material barriers: economic dependency, violence threat, institutional exclusion) versus internalized (cognitive patterns: identity fusion, self-blame, normalized obligatedness)?',
    'Post-exit trajectory analysis: measure suppression persistence after material barriers are removed. If suppression continues (self-blame narratives, identity fragmentation, continued deference patterns), it indicates internalized component. If suppression resolves with barrier removal, primarily structural.',
    'If primarily internalized: the constraint''s effective suppression is higher than structural metrics suggest — the target carries suppression into all future contexts. If primarily structural: targeted material support (housing, childcare, economic transition) may be sufficient for exit. Mixed mechanisms require dual interventions (material support + identity reframing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_constraint_ambiguity, empirical, 'Structural vs internalized suppression mechanisms').

omega_variable(
    identity_fusion_mechanism_specificity,
    'What specific identity-fusion mechanisms bind the conditioned subject to the patriarchal arrangement? Is it fusional attachment to the authority holder (relational identity), internalized gendered self-concept (self-identity), ideological commitment to patriarchal worldview (epistemic identity), or institutional role ossification (institutional identity)?',
    'Qualitative analysis of exit narratives and identity-dissolution experiences. Differential dissolution rates: relational identity fuses fastest, dissolves hardest; self-identity fuses through repeated socialization, dissolves through identity-relevant role availability; epistemic identity fuses through narrative naturalization, dissolves through counter-narrative exposure; institutional identity fuses through role tenure, dissolves through institutional change.',
    'If primarily relational: interventions must include relationship reconstruction and new attachment pathways. If primarily self-concept: identity reframing and role modeling central. If primarily epistemic: counter-narratives and consciousness-raising. If primarily institutional: structural role availability (economic opportunity, professional access, political participation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_fusion_mechanism_specificity, empirical, 'Specific mechanisms of identity fusion in patriarchal conditioning').

omega_variable(
    cyclical_reinforcement_vs_cumulative_extraction,
    'Is patriarchal conditioning maintained through intermittent reinforcement cycles (tension-incident-reconciliation-calm-tension, sustaining the lock through variable reward schedules) or through cumulative extraction trajectories (extraction deepens over time, lock intensifies)?',
    'Measurement longitudinal analysis: does extractiveness oscillate cyclically or drift upward? Do therapy/narrative data show consistent cycle patterns or progressive entrapment deepening? Do exit attempts cluster at crisis points (high tension) or throughout the cycle?',
    'If intermittent reinforcement dominates: the constraint is a variable-reward schedule (powerful psychological lock, similar to gambling/trauma bonding). If cumulative extraction dominates: the lock intensifies over time, making exit increasingly costly. Mixed mechanism suggests different subpopulations experience different dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyclical_reinforcement_vs_cumulative_extraction, empirical, 'Reinforcement mechanism: intermittent vs cumulative').

omega_variable(
    institutional_vs_interpersonal_enforcement_coupling,
    'How much of the observed suppression (0.72) derives from institutional apparatus enforcement (legal, economic, religious, educational systems) versus interpersonal enforcement (authority holder behavior, peer group conformity pressure, family system dynamics)?',
    'Comparative analysis: suppression levels in high-institutional-enforcement contexts (theocratic states, legally restricted property rights, educational access barriers) vs low-institutional-enforcement contexts (legal gender equality, economic access, education universality). If institutional apparatus decoupling produces suppression drop ≥ 0.30, institutional enforcement is primary.',
    'If primarily institutional: legal reform, institutional access expansion, and enforcement targeting are central interventions. If primarily interpersonal: relationship dynamics work, consciousness-raising, and social norm shifting are central. Strong coupling suggests institutional and interpersonal reinforcement are mutually dependent — dismantling either destabilizes both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_interpersonal_enforcement_coupling, empirical, 'Institutional vs interpersonal enforcement coupling').

omega_variable(
    feminist_counter_coalition_sunset_viability,
    'Are the feminist counter-coalition''s institutional changes (education access, economic participation, legal reform, representation) creating genuine alternative structures (scaffold sunset) or are patriarchal extraction mechanisms adapting and reproducing through updated forms (wage gap, care work devaluation, harassment, reproductive coercion)?',
    'Longitudinal institutional tracking: measure extraction rates over 50+ year horizon across legal-reform and education-access expansion jurisdictions. Do extraction mechanisms persist with new forms (e.g., ''double shift'' phenomenon of paid work + household labor) or do they genuinely decline? Do women''s exit rates from patriarchal relationships increase as institutional barriers fall?',
    'If sunset is real: scaffold perspective is structurally accurate and extractive mechanisms will decline with time and institutional change. If mechanisms adapt: the snare is more robust than the scaffold perspective assumes — institutional change alone is insufficient and identity-lock mechanisms must also be addressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feminist_counter_coalition_sunset_viability, empirical, 'Feminist institutional reform success: sunset vs adaptation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patriarchal_psychological_conditioning, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patri_tr_t0, patriarchal_psychological_conditioning, theater_ratio, 0, 0.65).
narrative_ontology:measurement(patri_tr_t5, patriarchal_psychological_conditioning, theater_ratio, 5, 0.74).
narrative_ontology:measurement(patri_tr_t10, patriarchal_psychological_conditioning, theater_ratio, 10, 0.81).
narrative_ontology:measurement(patri_tr_t15, patriarchal_psychological_conditioning, theater_ratio, 15, 0.76).

% Extraction over time
narrative_ontology:measurement(patri_be_t0, patriarchal_psychological_conditioning, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(patri_be_t5, patriarchal_psychological_conditioning, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(patri_be_t10, patriarchal_psychological_conditioning, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(patri_be_t15, patriarchal_psychological_conditioning, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patriarchal_psychological_conditioning, identity_coordination).
narrative_ontology:boltzmann_floor_override(patriarchal_psychological_conditioning, 0.12).
narrative_ontology:affects_constraint(patriarchal_psychological_conditioning, patriarchal_institutional_enforcement).
narrative_ontology:affects_constraint(patriarchal_psychological_conditioning, patriarchal_relational_coordination).
narrative_ontology:affects_constraint(patriarchal_psychological_conditioning, gendered_labor_extraction).
narrative_ontology:affects_constraint(patriarchal_psychological_conditioning, reproductive_autonomy_suppression).

% DUAL FORMULATION NOTE:
% Patriarchal conditioning decomposes into a constraint family: (1) patriarchal_psychological_conditioning (ε=0.68, identity-lock and psychological extraction, this file); (2) patriarchal_institutional_enforcement (ε=0.55, legal/economic/religious apparatus); (3) patriarchal_relational_coordination (ε=0.28, genuine household coordination usurped for extraction). Each has distinct ε, distinct beneficiary/victim structure, distinct measurement trajectory. The psychological conditioning is upstream of institutional enforcement (institution provides legitimacy for conditioning), and both affect relational coordination dynamics. Separate stories enable precise diagnosis of which intervention targets (consciousness-raising for psychology, institutional reform for apparatus, relational renegotiation for coordination) address which mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(patriarchal_psychological_conditioning, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
