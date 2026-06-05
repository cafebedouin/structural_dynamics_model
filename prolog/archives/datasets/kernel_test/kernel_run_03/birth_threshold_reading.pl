% ============================================================================
% CONSTRAINT STORY: birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birth_threshold_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: birth_threshold_reading
 *   human_readable: Birth Threshold: Parental Authority with Infant Personhood Entry at Live Birth
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   The birth-threshold reading of personhood establishes a sharp boundary:
 *   personhood begins at live birth, conferring legal status and protection
 *   against state eugenic administration. However, this reading
 *   simultaneously grants parental authority over exposure and rearing
 *   decisions, creating a tangled structure where newly conferred personhood
 *   coexists with vulnerability to parental discretion. This is one reading
 *   of the contested personhood-boundary kernel. The sibling readings — the
 *   inherent-dignity reading (personhood at conception or ensoulment) and the
 *   civic-eugenic reading (personhood conditional on state-determined eugenic
 *   fitness) — propose different boundaries and different authority
 *   structures. The birth-threshold reading's distinctive claim is that the
 *   biological event of birth provides a natural, legally administrable
 *   threshold for personhood entry, while explicitly foreclosing state
 *   eugenic authority over persons. The reading permits private parental
 *   authority (exposure, selective rearing) but forbids state authority
 *   (forced breeding, systematic elimination). This creates an asymmetry: the
 *   state is constrained while parents are empowered. The extractiveness
 *   profile reflects parental authority concentration (beneficiaries gain
 *   discretion over which infants are reared) combined with infant
 *   vulnerability (victims enter personhood but lack protection from parental
 *   discretion). Theater ratio measures the performative element of
 *   personhood conferral: the legal status is declared at birth but its
 *   substantive protections depend on enforcement against parental
 *   discretion, a mechanism often theatrical in effect.
 *
 * KEY AGENTS:
 *   - Live-Born Infants: Primary victims (powerless/trapped) — enter personhood at birth but dependent on parental discretion for survival; exposed to parental exposure authority; no agency to exit the relationship
 *   - Paterfamilias / Parental Authority Holders: Primary beneficiaries (institutional/arbitrage) — gain discretionary authority over infant fate (rearing, exposure, resource allocation); solve coordination problem of allocating resources across offspring
 *   - Neonatal Welfare Collective: Secondary actor (moderate/constrained) — includes midwives, kin networks, religious authorities, public health agents; constrained by parental prerogative but coordinating actual survival
 *   - State Eugenic Authority: Subordinated institutional actor (organized/constrained) — explicitly foreclosed from direct control over personhood determinations; cannot override an infant's right to live once born; constrained relative to paterfamilias
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the birth event as immutable boundary, obscuring that this is a contingent normative choice among competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birth_threshold_reading, 0.38).
domain_priors:suppression_score(birth_threshold_reading, 0.62).
domain_priors:theater_ratio(birth_threshold_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birth_threshold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(birth_threshold_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(birth_threshold_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birth_threshold_reading, tangled_rope).
narrative_ontology:human_readable(birth_threshold_reading, "Birth Threshold: Parental Authority with Infant Personhood Entry at Live Birth").
narrative_ontology:topic_domain(birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(birth_threshold_reading, '93e98690-8bcd-4123-bcfd-44fc13bb079b').
narrative_ontology:cs_created_at('93e98690-8bcd-4123-bcfd-44fc13bb079b', '').
narrative_ontology:cs_kernel_codification('93e98690-8bcd-4123-bcfd-44fc13bb079b', fixed_text).
narrative_ontology:cs_authority_grounding('93e98690-8bcd-4123-bcfd-44fc13bb079b', lineage).
narrative_ontology:cs_interpretation_layer_present('93e98690-8bcd-4123-bcfd-44fc13bb079b').
narrative_ontology:cs_kernel_id(birth_threshold_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('93e98690-8bcd-4123-bcfd-44fc13bb079b', inherent_dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('93e98690-8bcd-4123-bcfd-44fc13bb079b', civic_eugenic_reading, forecloses).
narrative_ontology:cs_axiom('93e98690-8bcd-4123-bcfd-44fc13bb079b', foundational, birth_event_is_personhood_threshold).
narrative_ontology:cs_axiom_status(birth_event_is_personhood_threshold, holdable).
narrative_ontology:cs_axiom_grounding('93e98690-8bcd-4123-bcfd-44fc13bb079b', birth_event_is_personhood_threshold, conventional).
narrative_ontology:cs_axiom('93e98690-8bcd-4123-bcfd-44fc13bb079b', foundational, parental_authority_supersedes_state_eugenic_authority).
narrative_ontology:cs_axiom_status(parental_authority_supersedes_state_eugenic_authority, holdable).
narrative_ontology:cs_axiom_grounding('93e98690-8bcd-4123-bcfd-44fc13bb079b', parental_authority_supersedes_state_eugenic_authority, deontological).
narrative_ontology:cs_reference_frame('93e98690-8bcd-4123-bcfd-44fc13bb079b', classical_paterfamilial_authority_with_birth_threshold).
narrative_ontology:cs_drift_state('93e98690-8bcd-4123-bcfd-44fc13bb079b', contemporary_human_rights_era, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(birth_threshold_reading, paterfamilias_authority).
narrative_ontology:constraint_beneficiary(birth_threshold_reading, parental_discretion_holders).
narrative_ontology:constraint_victim(birth_threshold_reading, live_born_infants).
narrative_ontology:constraint_victim(birth_threshold_reading, neonatal_welfare_collective).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIVE-BORN INFANT — Trapped by dependence and lack of agency. Enters personhood at birth but immediately subject to parental exposure authority (abandonment, denial of sustenance). No exit capacity; full exposure to parental discretion. Maximum experienced extraction despite newly conferred personhood status. The threshold grants legal status but not protection.
constraint_indexing:constraint_classification(birth_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: NEONATAL WELFARE COLLECTIVE — Constrained by resource barriers and institutional capacity limits. Coordinating infant survival requires distributed care (midwives, wet nurses, kin networks). But the coordination is embedded in asymmetric parental authority: the collective can intervene against infanticide only at cost to parental prerogative. Mixed coordination (legitimate need for parental judgment in resource allocation) and extraction (parental control over life/death decisions).
constraint_indexing:constraint_classification(birth_threshold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATERFAMILIAS — Institutional beneficiary experiencing pure coordination. The birth threshold reading grants paterfamilias discretionary authority: who lives in the household, which children to rear or expose, succession questions. The parent solves a genuine coordination problem: allocating scarce resources across offspring with heterogeneous viability and economic utility. Experiences constraint as enabling, not extractive.
constraint_indexing:constraint_classification(birth_threshold_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EUGENIC AUTHORITY / STATE APPARATUS — The birth threshold reading explicitly forecloses state eugenic authority: once born, the infant is a person with rights the state cannot override for breeding purposes. This perspective sees the constraint as a limitation (it prevents rational population management), but the reading denies this agent legitimate exit or alternative. The state has no arbitrage option here — parental discretion, not state discretion, governs. This perspective is subordinated, not beneficiary.
constraint_indexing:constraint_classification(birth_threshold_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW — From civilizational scope, birth-threshold personhood can appear as a natural fact: personhood emerges at the biological event of birth (first breath, cord severance, emergence from the womb). The threshold appears as an immutable natural boundary. However, the structural data reveals this as a false summit: the reading is one interpretation among siblings with equal logical coherence. Naturalization of the birth event obscures the reading's dependence on a specific normative choice about what moral status is granted at birth.
constraint_indexing:constraint_classification(birth_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birth_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(birth_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birth_threshold_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading grants parental authority over infant fate, which is genuine beneficiary concentration — parents can choose which children to rear and which to expose, capturing resource allocation decisions and succession control. However, extractiveness is not severe because the coordination problem is real: allocating scarce resources across heterogeneous offspring is genuinely difficult, and parental judgment may be more efficient than state or collective allocation. The extracted benefit is partial payment for the coordination service provided. Suppression (0.62): High. Infants are physically helpless and dependent on parental provision. Suppression operates through biological necessity (infants cannot exit the parental relationship, cannot feed themselves, cannot advocate). The threshold grants legal status (personhood) but provides minimal practical protection against this biological suppression. Theater ratio (0.55): Moderate-High. Personhood conferral at birth is partly performative — legal status is declared but enforcement requires constant vigilance against parental discretion. The performative element increases over the measurement interval as explicit enforcement mechanisms (legal doctrine, inspection procedures, institutional oversight) accumulate without fundamentally changing parental authority. The theater reflects the gap between the reading's claim (infants are persons) and the institutional reality (parents retain near-total discretion).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the beneficiary (paterfamilias, rope perspective) and the primary victim (infant, snare perspective). The parent experiences the constraint as solving a genuine coordination problem — how to allocate parental investment across heterogeneous offspring with different survival chances and economic utility. From the parent's vantage point, exposure is a legitimate exit option when resources cannot support all children. The infant experiences the same constraint as pure extraction: conferral of personhood status without substantive protection against parental abandonment or denial of sustenance. The state eugenic authority perspective reveals a secondary gap: the reading forecloses direct state authority but does not address whether eugenic outcomes can be achieved through parental incentivization. The analytical observer at civilizational scope risks a false-summit classification: treating the birth event as an immutable natural law rather than a contingent normative reading. This gap between beneficiary coordination experience and victim extraction experience is the signature feature of tangled rope — the constraint simultaneously solves a real problem (resource allocation) and creates asymmetric harm (infant vulnerability).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (paterfamilias) experience low effective extraction because they hold arbitrage options: they can exit this particular child's rearing by exposure, can reallocate resources, can negotiate with kin. Their d value is low (~0.20), producing negative or low χ — they experience the constraint as enabling coordination, not extractive. Victims (infants) experience maximum effective extraction because they are trapped: no exit capacity, dependence on parental discretion, vulnerability to death or harm. Their d value is high (~0.92), producing high χ despite moderate base extractiveness — trapped powerless agents experience whatever extraction exists more severely than institutional agents with exit options. The neonatal welfare collective occupies the middle: they have some agency (can coordinate care networks, can intervene against extreme abuse) but are constrained by parental prerogative. Their d value is moderate (~0.60), producing moderate χ and tangled-rope classification. The state eugenic authority perspective, subordinated by the reading's core axiom (foreclosure of state authority over persons), shows constrained exit (can only work through incentive structures, not direct authority) with high d (~0.78), experiencing the constraint as extractive to their institutional interests despite having no victimhood claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the kernel structure: this reading is one position in a three-way contest over where personhood begins and who holds authority over it. The reading claims that birth provides a natural, administrable threshold and that parental (not state) authority governs post-birth fate. This forecloses the civic-eugenic reading (no state authority) and influences but does not foreclose the inherent-dignity reading (which might place personhood earlier and derive different authority implications). The reading's mandatrophy — why does personhood entry (which should be natural law) correlate with parental authority (which is clearly contingent institutional structure)? — is resolved by acknowledging that 'personhood' in this reading is a legal status conferred at a biological event, not a natural fact independent of social interpretation. The extractiveness (0.38) reflects not failure to classify but the mixed structure: genuine coordination function + asymmetric authority concentration. Tangled rope is the correct classification precisely because both elements are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viable_infant_survival_baseline,
    'At what baseline viability threshold (gestational age, anatomical completeness, respiratory capacity) does the birth-threshold reading grant personhood status? Is the threshold strict (birth event alone) or continuous (with earlier cutoffs for viable premature infants)?',
    'Historical and contemporary medical definitions of live birth across jurisdictions; analysis of whether premature infants born before anatomical completion receive same personhood as term infants; examination of whether ''born'' requires independent respiration or exits the womb.',
    'If strict birth-event threshold: boundary is clear but excludes some physiologically continuous beings. If viability-continuous: boundary dissolves into gradation, creating ambiguity identical to the conception-reading siblings. The reading''s coherence depends on maintaining a sharp boundary; continuous viability undermines it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(viable_infant_survival_baseline, empirical, 'Biological definition of live birth and its relationship to viability').

omega_variable(
    parental_discretion_scope_limits,
    'Does parental exposure authority extend to all post-birth decisions (feeding, shelter, medical intervention, education) or only to initial abandonment/non-rearing? Where does parental discretion end and infant personhood rights begin?',
    'Historical case law and customary practice regarding parental authority over neonates; analysis of which parental acts trigger legal intervention (starvation, abuse, denial of medical care); comparison across jurisdictions with birth-threshold readings.',
    'If parental discretion is near-total: infants are persons legally but powerless practically (snare from infant perspective). If discretion is limited: personhood grants substantive protections (moderate tangled rope for infants). The reading''s internal coherence depends on resolving where exposure authority ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_discretion_scope_limits, conceptual, 'Scope of parental exposure authority over live-born infants').

omega_variable(
    kernel_reading_contest_structure,
    'What makes this reading — birth-threshold personhood with parental exposure authority — distinct from the inherent-dignity and civic-eugenic sibling readings? Which foundational normative claim distinguishes this reading''s legitimacy grounding?',
    'Textual analysis of foundational documents (ancient law codes, theological treatises, modern constitutions) that adopt birth-threshold vs. conception-threshold vs. state-rationality-threshold personhood; identification of which axioms each reading depends on; examination of which sibling reading foreclosures are logically necessary vs. pragmatically convenient.',
    'If the reading''s distinctiveness is empirical (when does personhood biologically emerge): empirical refutation of the birth event as unique threshold could foreclose this reading, driving reclassification toward continuous emergence or state-determined boundaries. If distinctiveness is deontological (personhood is a moral status conferred at birth): the reading''s coherence is resistant to empirical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Distinguishing axioms of birth-threshold reading vs. sibling readings').

omega_variable(
    state_foreclosure_robustness,
    'Does the birth-threshold reading genuinely foreclose state eugenic authority, or does it merely subordinate state authority to parental authority, leaving eugenic goals achievable through parental incentive structures?',
    'Historical analysis of whether states using birth-threshold personhood have implemented eugenic programs through tax incentives, subsidy allocation, or family-planning coercion that achieve eugenic outcomes without explicit state authority over persons. Examination of whether foreclosure is logical (states cannot override a person''s rights) or merely practical (states lack direct control mechanisms).',
    'If foreclosure is only practical: eugenic authority persists through parental incentivization, making the ''foreclosure'' nominal. If foreclosure is logical: state eugenic authority is genuinely impossible under this reading. The reading''s claim to have excluded eugenic power depends on the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_foreclosure_robustness, empirical, 'Whether birth-threshold reading logically forecloses state eugenic authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birth_threshold_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birt_tr_t0, birth_threshold_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(birt_tr_t3, birth_threshold_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(birt_tr_t6, birth_threshold_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(birt_be_t0, birth_threshold_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(birt_be_t3, birth_threshold_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(birt_be_t6, birth_threshold_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birth_threshold_reading, resource_allocation).
narrative_ontology:affects_constraint(birth_threshold_reading, inherent_dignity_reading).
narrative_ontology:affects_constraint(birth_threshold_reading, civic_eugenic_reading).

% DUAL FORMULATION NOTE:
% The personhood boundary is a contested kernel with three structurally distinct readings. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and authority configuration. This story models the birth-threshold reading (ε=0.38, parental authority beneficiary, state eugenic authority foreclosed). The inherent-dignity and civic-eugenic readings are separate constraint stories with different ε values reflecting their different empirical and normative claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(birth_threshold_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
