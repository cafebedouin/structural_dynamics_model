% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_congressional_primacy, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: War Powers Allocation: Congressional Primacy Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The war powers allocation constraint concerns the constitutional
 *   distribution of authority to initiate military force. Article I, Section
 *   8 vests in Congress the power to declare war; Article II vests in the
 *   President the title of Commander-in-Chief. This reading instantiates the
 *   congressional primacy interpretation: military force beyond immediate
 *   defense requires explicit congressional authorization as a constitutional
 *   necessity. The constraint is actively enforced through constitutional
 *   doctrine, statutory obligation (War Powers Resolution 1973), and
 *   appropriations leverage, but the functional allocation has eroded through
 *   accumulated precedent and executive doctrinal assertion. Military action
 *   in Korea (1950, no declaration), Vietnam (1964, Tonkin Gulf resolution),
 *   Iraq (1991, authorized; 2003, authorization framed as AUMF update),
 *   Afghanistan (2001, authorized), Syria (2014–present, conducted under 2001
 *   AUMF), and Yemen (ongoing, no explicit authorization) show increasing
 *   extraction of the declaration power from Congress. The constraint's
 *   extractiveness has risen from 0.25 (1946: formal declarations norm) to
 *   0.58 (2023: unilateral action expected, authorization is post-hoc cover).
 *   Theater has risen from 0.15 to 0.60 as the War Powers Resolution's
 *   consultation ritual has become performative. Suppression has risen from
 *   0.20 to 0.65 as executive doctrinal claims (commander-in-chief supremacy,
 *   state necessity, inherent authority) must actively suppress competing
 *   constitutional readings. This story instantiates one reading of the
 *   contested kernel war_powers_allocation. The sibling readings are the
 *   inherent_executive_reading (Article II grants president unilateral
 *   war-making authority) and the functional_accommodation_reading (some
 *   balance between executive speed and congressional constraint is
 *   constitutionally necessary). This reading holds that Congress's
 *   declaration authority is foundational, not subordinate to executive
 *   necessity.
 *
 * KEY AGENTS:
 *   - Congress (Legislative Branch): Victim and ostensible authority holder (Article I, Section 8) — bears extraction as unilateral executive action undermines constitutional grant
 *   - President / Executive Branch: Beneficiary and active extractor (organized/arbitrage) — captures war-making initiative; experiences constraint as coordination opportunity rather than constraint
 *   - Courts / Judicial Branch: Secondary actor (institutional/arbitrage) — interprets Constitution; has consistently deferred to executive war-making claims under state-necessity and commander-in-chief doctrines; reinforces extraction through non-enforcement of WPR
 *   - War Powers Resolution (1973): Institutional mechanism (institutional/constrained) — attempts to restore congressional constraint through reporting and 60-day clock; functions as theater rather than genuine constraint due to ambiguous enforceability
 *   - Congress as Organized Coalition: Can be modeled as organized agent (organized/constrained) — retains appropriations leverage; can constrain through budget riders, defunding threats, procedural delays; but exit cost is political (troops already deployed)
 *   - Analytical Observer: Sees structural constraint disguised as natural law (analytical/analytical) — risks naturalizing contingent institutional arrangement as necessary feature of state survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.58).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "War Powers Allocation: Congressional Primacy Reading").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'ecdf10a0-1978-46c2-b38f-7485406b4b10').
narrative_ontology:cs_kernel_codification('ecdf10a0-1978-46c2-b38f-7485406b4b10', fixed_text).
narrative_ontology:cs_authority_grounding('ecdf10a0-1978-46c2-b38f-7485406b4b10', lineage).
narrative_ontology:cs_interpretation_layer_present('ecdf10a0-1978-46c2-b38f-7485406b4b10').
narrative_ontology:cs_reading_relation('ecdf10a0-1978-46c2-b38f-7485406b4b10', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecdf10a0-1978-46c2-b38f-7485406b4b10', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('ecdf10a0-1978-46c2-b38f-7485406b4b10', foundational, congressional_declaration_is_constitutionally_required_for_war).
narrative_ontology:cs_axiom_status(congressional_declaration_is_constitutionally_required_for_war, holdable).
narrative_ontology:cs_axiom_grounding('ecdf10a0-1978-46c2-b38f-7485406b4b10', congressional_declaration_is_constitutionally_required_for_war, deontological).
narrative_ontology:cs_axiom('ecdf10a0-1978-46c2-b38f-7485406b4b10', secondary, executive_unilateral_action_is_extractive_when_sustained).
narrative_ontology:cs_axiom_status(executive_unilateral_action_is_extractive_when_sustained, holdable).
narrative_ontology:cs_axiom_grounding('ecdf10a0-1978-46c2-b38f-7485406b4b10', executive_unilateral_action_is_extractive_when_sustained, empirically_contingent).
narrative_ontology:cs_reference_frame('ecdf10a0-1978-46c2-b38f-7485406b4b10', constitutional_design_formal_declaration_regime).
narrative_ontology:cs_drift_state('ecdf10a0-1978-46c2-b38f-7485406b4b10', contemporary_unilateral_executive_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ecdf10a0-1978-46c2-b38f-7485406b4b10', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress_institutional_authority).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress_war_declaration_power).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, legislative_constraint_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESS AS INSTITUTIONAL VICTIM (SNARE) — The constitutional authority to declare war is structurally suppressed. Executive unilateral military action (Korea 1950, Vietnam 1964, Iraq 2003) extracts the declaration power from Congress without formal amendment. Congress cannot exit this constraint — the supermajority requirement for constitutional amendment (3/5 of states) is insurmountable. Each extraction (military commitment without declaration) establishes precedent weakening the next congressional authorization claim. Theater of consultation (War Powers Resolution §3 reporting) creates appearance of constraint without restoration of power.
constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONGRESS AS ORGANIZED COALITION (TANGLED ROPE) — Congress retains genuine coordination function: appropriations for military forces require legislative votes (Article I, Section 8), and budget leverage provides real constraint on executive duration and scope of military action. However, coordination is asymmetric: presidents use emergency powers and existing appropriations to initiate action; Congress votes to continue or defund after commitment is sunk. The exit cost is high (cutting off troops mid-deployment is politically suicidal), creating captured coordination where Congress endorses extraction.
constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (ROPE) — From the executive perspective, the constraint functions as coordination: consultation with Congress (informal or formal) enables legitimacy, burden-sharing, and political insulation. The War Powers Resolution §3 (notification requirement) and Article II (Commander-in-Chief clause) together create a coordination equilibrium where the president exercises military authority while Congress retains budgetary and appropriations leverage. The executive experiences this as pure coordination — solving the collective action problem of responding quickly to military necessity while maintaining civilian control. The executive's structural position (arbitrage: can appeal to war necessity, existing appropriations, precedent) is sharply asymmetric to Congress's constrained position.
constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WAR POWERS RESOLUTION RITUAL (PITON) — The War Powers Resolution (1973) is a degraded constraint on executive war power. Section 3 requires notification 'in every possible instance' of armed force commitment. Section 5 requires congressional authorization within 60 days or the armed force must be removed. However: (a) every administration since 1973 has disputed the constitutionality of the statute, treating notification as courteous rather than binding; (b) no president has complied with the 60-day clock; (c) Congress has authorized continued force through appropriations rider rather than explicit war votes, converting the constraint into a theater of post-hoc legitimation. The 'consultation' ritual persists because formal repeal would be politically impossible, but the functional constraint has eroded. Theater ratio (0.62) reflects this degradation.
constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal lens, some degree of executive war-making capacity is inherent to state survival: standing armies require continuous command authority, and waiting for legislative votes during sudden attack is tactically impossible. This perspective naturalizes the gap between constitutional text (Congress declares war) and operational reality (presidents direct military action) as an unavoidable feature of state structure. However, the structural data contradicts mountain classification: the constraint's beneficiaries (executive branch), its suppression mechanism (accumulated precedent), and its theater component (War Powers Resolution ritual) all reveal this as a false summit — a contingent institutional arrangement naturalized as physical law.
constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: FUNCTIONAL ACCOMMODATION VIEW (TANGLED ROPE) — A powerful actor (e.g., a sophisticated executive advisor or institutional designer) that accepts the functional necessity of executive military authority but seeks to preserve congressional constraint through procedural enforcement can read this constraint as tangled rope at generational timescale: congress loses immediate veto but gains generational check through appropriations control and constitutional amendment threat. This perspective has mobile exit options: it can shift the equilibrium through institutional redesign (e.g., empowering War Powers Resolution enforcement, constitutional amendment, or selective defunding). The classification reflects a different institutional reading that coexists with the congressional primacy reading.
constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_powers_allocation__congressional_primacy_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The president captures substantial authority to initiate military force without explicit congressional declaration. The extraction is not complete (Congress retains appropriations leverage and can theoretically defund or force withdrawal) but is substantial and ratcheting. The measurement trajectory shows extraction rising from 0.25 to 0.58 over the 1946–2023 interval, with acceleration post-1973 as precedent accumulates. The congressional primacy reading measures extractiveness relative to the constitutional baseline (Congress declares war, Article I, Section 8) — the gap between that text and current practice is the extraction. Suppression (0.68): Moderate-high. Executive doctrinal claims (commander-in-chief supremacy, state necessity, sole-organ theory, inherent authority) suppress alternative constitutional readings. Courts have consistently deferred to executive war-making claims and declined to enforce War Powers Resolution. Congressional dissent exists but faces high coordination costs and faces legitimacy challenges from precedent (prior authorizations, etc.). Theater ratio (0.62): Moderate. War Powers Resolution §3 notification and §5 authorization-seeking create appearance of constraint without substance. Presidential consultation with Congress is ritualistic; appropriations votes are typically sunk-cost captures rather than de novo authorization decisions. The theater has risen from 0.15 to 0.62 as formal declarations (which carried genuine constitutional weight) have been replaced with authorization-updating and consultation rituals. Beneficiaries: Congress's declaration authority is the nominal beneficiary of the constraint (Congress is meant to be empowered), but the structural data shows extraction flows away from Congress — the constraint protects an authority Congress is meant to exercise. We list 'congress_institutional_authority' as the beneficiary in the abstract sense, but the actual beneficiary is the Executive (which benefits from unfettered ability to commit force) and the principal victim is Congress (whose authority is suppressed). This apparent contradiction is resolved by noting that the constraint is named for what it purports to protect (congressional primacy) but is actually a mechanism for extracting that protection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap between congressional and executive readings. Congress perceives a Snare: trapped in constitutional text, unable to enforce the declaration power, losing authority through accumulated precedent, facing suppression of alternative constitutional readings. The executive perceives a Rope: solving the coordination problem of responding to military necessity while maintaining civilian oversight through appropriations and consultation. The analytical observer risks perceiving a Mountain (war-making gap is inherent to state structure) but the structural data reveals this as a false summit: the beneficiaries (executive), the suppression mechanism (doctrinal claims and judicial deference), and the theater (WPR ritual) all show this is a contingent institutional arrangement, not a law of nature. The piton perspective (WPR as degraded ritual) reflects that the attempted constraint has lost functional force and persists through institutional inertia. The rival 'functional accommodation' reading coexists as a live position in some judicial and executive circles, creating a multi-position landscape where the same constitutional text supports contradictory readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the extraction flow. Congress, as the victim ('trapped' in constitutional text but unable to enforce it), has d ≈ 0.85–0.95, mapping to high f(d) and high experienced extractiveness (χ). The executive, as the beneficiary (arbitrage exit: can commit force unilaterally and rely on sunk-cost appropriations), has d ≈ 0.05–0.15, mapping to low/negative f(d) and low experienced extractiveness. The analytical observer, seeing both positions, has d ≈ 0.73 (canonical for analytical) but risks misclassifying as Mountain (natural law) when the structural data shows Tangled Rope (mixed coordination and extraction). The piton perspective (War Powers Resolution theater) derives from the high theater gate and moderate extraction — the ritual persists through institutional inertia despite eroded functional constraint. The 'functional accommodation' rival reading places a powerful actor in mobile position with generational timescale, lowering experienced extraction and creating a Tangled Rope classification from a different institutional position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (executive + Congress share military necessity problem) from extraction (executive captures unilateral authority while Congress retains only leverage, not power). The executive's Rope reading is the genuine coordination function: presidential speed + Congressional constraint through appropriations is a defensible equilibrium. Congress's Snare reading is the extraction: Congress loses decision authority over war initiation while retaining only post-hoc approval votes that are sunk-cost captures. The Tangled Rope classification (claimed type) reflects both: there is real coordination (executive cannot sustain force without congressional appropriations) and real extraction (Congress cannot prevent initiation, only defund continuation at high political cost). The mandatrophy is resolved by noting that the constraint's primary functional purpose (coordinate military response while preserving civilian control through legislative oversight) has been captured by the extraction mechanism (executive unilateral initiation with post-hoc congressional sunk-cost approval).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_amendment_threshold,
    'Is the supermajority requirement for constitutional amendment (27 states'' ratification of Article V amendment) a genuine exit option for Congress or a theoretical constraint that has never successfully limited war-making?',
    'Historical analysis of amendment attempts post-1950 (e.g., Bricker Amendment, War Powers Resolution as statutory alternative); assessment of whether amendment pathway is genuinely available or foreclosed by political concentration',
    'If amendment is foreclosed in practice: Congress''s exit is ''trapped'' (not merely ''constrained''), strengthening Snare classification from Congress''s perspective. If amendment remains available: ''constrained'' exit classification is accurate, supporting Tangled Rope rather than pure Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_threshold, empirical, 'Whether constitutional amendment is a genuine exit option for Congress or politically foreclosed').

omega_variable(
    precedent_accumulation_mechanism,
    'Does each instance of unilateral executive military action (Korea, Vietnam, Iraq, Syria, Yemen) create binding precedent that weakens subsequent congressional authorization claims, or do courts/Congress treat each instance as independent?',
    'Doctrinal analysis of judicial deference to executive war-making; review of congressional rhetoric pre/post each military action; tracking of authorization-seeking behavior (whether presidents seek Congressional authorization after baseline precedent is set)',
    'If precedent accumulates (each action makes next action easier): extractiveness is ratcheting upward, suppression increases over time, theater becomes more necessary. If each instance is independent: extractiveness is stable, not trending toward greater executive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_accumulation_mechanism, empirical, 'Whether unilateral military actions create binding precedent that weakens congressional authority').

omega_variable(
    war_powers_resolution_enforceability,
    'Is the War Powers Resolution (1973) enforceable as written, or does its language permit sufficient executive interpretation that it functions as theater rather than constraint?',
    'Textual analysis of ''every possible instance'' notification requirement and 60-day clock; comparison of presidential compliance across administrations; review of court cases (Holtzman v Schlesinger, Raines v Byrd) assessing justiciability and standing',
    'If enforceable: War Powers Resolution is genuine Scaffold with sunset clause (60-day removal mandate) and measured theater (0.62 is accurate). If non-enforceable: WPR is pure Piton, and suppression should be higher (0.75+), extractiveness higher (0.65+), because the apparent constraint provides only cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(war_powers_resolution_enforceability, empirical, 'Whether War Powers Resolution provides enforceable constraint or functions as theater').

omega_variable(
    congressional_authorization_as_capture,
    'When Congress votes to authorize continued military force (as in Iraq 2003, Afghanistan 2001, 2015 AUMF votes), is this a genuine coordinate decision or a captured response to sunk-cost president?',
    'Analysis of legislative debate timing (how much debate before troops deployed?), vote distribution (margin of approval), and counterfactual (would Congress have authorized if presented de novo without deployed forces?); comparison to declaration-of-war era voting patterns',
    'If authorization votes reflect genuine legislative judgment: Congress''s ''constrained'' exit classification is accurate, and the constraint is Tangled Rope (mixed coordination and extraction). If votes reflect sunk-cost capture: Congress''s exit is forced (''trapped''), and the constraint is Snare with congressional as victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_authorization_as_capture, empirical, 'Whether congressional war authorizations reflect genuine legislative judgment or sunk-cost capture').

omega_variable(
    reading_foreclosure_scope,
    'Does the congressional primacy reading logically foreclose the inherent executive reading, or can a coherent interpreter hold both as coexisting readings of Article II?',
    'Jurisprudential analysis: does affirmation that Congress has constitutional authority to declare war necessarily deny that the president has inherent authority to direct military operations as commander-in-chief? Does Article II textualism require choosing one reading over the other?',
    'If foreclosure is genuine: reading_relation is ''forecloses'' (primary logic gate is exclusive). If readings coexist: reading_relation is ''coexists_with'' (both readings live in different judicial/political factions). Classification cascades from this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_scope, conceptual, 'Whether congressional primacy reading logically forecloses inherent executive authority reading').

omega_variable(
    functional_necessity_vs_institutional_design,
    'Is the observed gap between constitutional text (Congress declares war) and operational reality (presidents unilaterally commit force) a necessary feature of state survival, or a contingent result of American institutional design choices?',
    'Comparative constitutional analysis: how do other democracies (UK, Germany, Australia) handle executive war-making constraints? Do they survive without presidential unilateral authority? Can US design implement greater congressional constraint without sacrificing defense capability?',
    'If gap is necessary: Mountain classification from analytical perspective is justified, and the constraint is genuinely natural law (not false summit). If gap is contingent: False Summit signature fires, revealing that ''inherent to state structure'' is a beneficiary narrative, and the constraint is a Snare or Tangled Rope despite appearing natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_vs_institutional_design, empirical, 'Whether war-making gap is structurally necessary or contingent institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warpower_tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(warpower_tr_t1, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(warpower_tr_t2, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(warpower_tr_t3, war_powers_allocation__congressional_primacy_reading, theater_ratio, 3, 0.62).
narrative_ontology:measurement(warpower_tr_t4, war_powers_allocation__congressional_primacy_reading, theater_ratio, 4, 0.62).
narrative_ontology:measurement(warpower_tr_t5, war_powers_allocation__congressional_primacy_reading, theater_ratio, 5, 0.6).

% Extraction over time
narrative_ontology:measurement(warpower_be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(warpower_be_t1, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(warpower_be_t2, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(warpower_be_t3, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(warpower_be_t4, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(warpower_be_t5, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(warpower_su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(warpower_su_t1, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1, 0.45).
narrative_ontology:measurement(warpower_su_t2, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2, 0.6).
narrative_ontology:measurement(warpower_su_t3, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(warpower_su_t4, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(warpower_su_t5, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 5, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_resolution_enforceability).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, congressional_appropriations_leverage).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel generates three distinct constraint stories corresponding to three structural readings of the Articles I and II text. The congressional_primacy_reading (this story) measures extractiveness relative to Congress's constitutional authority baseline; the inherent_executive_reading measures from executive constitutional baseline; the functional_accommodation_reading treats both as boundary conditions on an equilibrium. Each reading has its own ε, beneficiary/victim structure, and set of perspectives. They are linked by the network structure because each reading influences the others' interpretive context and doctrinal development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, institutional, 0.12).
constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
