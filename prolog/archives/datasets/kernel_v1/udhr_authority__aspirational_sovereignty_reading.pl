% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR as Aspirational Moral Guidance (State Sovereignty Reading)
 *   domain: international_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   The Universal Declaration of Human Rights (UDHR, 1948) constitutes a
 *   contested kernel — a single institutional artifact (the Declaration text)
 *   that different parties read through fundamentally different frameworks,
 *   producing structurally distinct constraints with different extractiveness
 *   values. This constraint story instantiates ONE reading: the
 *   aspirational_sovereignty_reading, which interprets UDHR as providing
 *   moral guidance that requires explicit state consent for binding
 *   obligation. From this reading's perspective, UDHR is a Rope constraint:
 *   states voluntarily coordinate around shared moral language without
 *   surrendering autonomy. However, this reading coexists with two siblings:
 *   the binding_universalism_reading (which treats UDHR obligations as
 *   universal and binding independent of state consent, making this a
 *   false-summit Mountain) and the customary_emergence_reading (which argues
 *   that widespread state practice has transformed UDHR aspirations into
 *   customary international law, making this a Tangled Rope or Snare for
 *   non-ratifying states). The reading choice is not neutral — it reflects
 *   and reinforces state-centered international law doctrine and benefits
 *   powerful states retaining veto power over binding obligations.
 *
 * KEY AGENTS:
 *   - Sovereign States (Treaty Ratifiers): Powerful institutional actors (institutional/arbitrage) — experience UDHR as coordination enabling negotiation; retain full veto over binding commitments; primary beneficiaries of low-extractiveness reading
 *   - Vulnerable Populations (Rights Bearers): Powerless individuals in non-ratifying or non-complying states (powerless/trapped) — aspiration-bound, receive moral promise without enforcement, primary victims of aspiration-obligation gap
 *   - Human Rights Treaty Bodies (UN HRC, Regional Courts): Institutional actors (institutional/constrained) — coordinate state behavior through persuasion while extracting authority through interpretation; occupy hybrid coordination-extraction position
 *   - Civil Society Advocacy Coalition (NGOs, Human Rights Networks): Organized agents (organized/constrained) — use UDHR as temporary scaffolding to negotiate binding treaty obligations; constrained but agentic
 *   - International Law Doctrinal Community (Scholars, Commentators): Institutional commentators (institutional/analytical) — produce and defend reading choices; disproportionately represent state-consent doctrine
 *   - Analytical Observer (Civilizational/Universal Perspective): Observational stance (analytical/analytical) — risks naturalizing this reading as universal moral truth, masking its role in naturalizing state sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.28).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.32).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, rope).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR as Aspirational Moral Guidance (State Sovereignty Reading)").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/human_rights/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, 'b08374b1-1f5e-4d0d-ab7e-8683b8541c52').
narrative_ontology:cs_kernel_codification('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', fixed_text).
narrative_ontology:cs_authority_grounding('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', lineage).
narrative_ontology:cs_interpretation_layer_present('b08374b1-1f5e-4d0d-ab7e-8683b8541c52').
narrative_ontology:cs_reading_relation('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', udhr_authority__customary_emergence_reading, coexists_with).
narrative_ontology:cs_axiom('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', foundational, state_consent_requirement_for_binding).
narrative_ontology:cs_axiom_status(state_consent_requirement_for_binding, holdable).
narrative_ontology:cs_axiom_grounding('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', state_consent_requirement_for_binding, conventional).
narrative_ontology:cs_axiom('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', foundational, aspiration_precedes_obligation).
narrative_ontology:cs_axiom_status(aspiration_precedes_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', aspiration_precedes_obligation, instrumental).
narrative_ontology:cs_reference_frame('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', state_veto_international_law).
narrative_ontology:cs_drift_state('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', contemporary_tribunal_jurisprudence_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b08374b1-1f5e-4d0d-ab7e-8683b8541c52', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, states_retaining_veto_power).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, treaty_negotiating_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVEREIGN STATE (ROPE) — Experiences UDHR as pure coordination: a moral reference frame that enables interstate dialogue without coercive obligation. State retains full veto over binding commitments. Theater exists (performative endorsement of aspirational norms) but serves coordination function — states use UDHR language to negotiate specific treaties. Low effective extraction because state has exit at every binding decision point.
constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: VULNERABLE POPULATION (SNARE) — Faces a trap: UDHR promises moral protection without enforcement. Individual rights holders in non-ratifying states or in states that ratify but don't implement have no remedy. The constraint extracts hope (emotional investment in aspirational claims) while supplying no material protection. High suppression because the vulnerable cannot force state ratification or compliance; exit is impossible. UDHR's moral framing prevents recognition of this extraction as such — the document naturalizes the gap between aspiration and obligation.
constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: TREATY BODY INSTITUTIONAL ACTOR (TANGLED ROPE) — Occupies hybrid position: treaty bodies (HRC, regional courts) coordinate state behavior through persuasion and reputational pressure (rope function) while simultaneously extracting authority from states that have ratified binding covenants. Constrained exit because treaty bodies' power derives entirely from state consent — states can withdraw from protocols or ignore rulings. But during the commitment period, treaty bodies exercise institutional extraction: they interpret UDHR and its derivative treaties to expand their own authority and constrain state discretion. Moderate extractiveness (0.35–0.45) reflects genuine coordination mixed with institutional empire-building.
constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: UDHR INSTITUTIONAL LEGACY (PITON) — At civilizational timescale, UDHR's primary function (coordinating a post-WWII international consensus on human dignity) has largely atrophied. The document persists through institutional inertia: cited in constitutions, reproduced in human rights education, invoked in diplomatic discourse, but functionally superseded by binding treaties (ICCPR, regional covenants). Theater ratio high (0.58) because much invocation of UDHR is performative — states and institutions cite it to legitimate binding obligations that actually derive from other sources. The constraint remains because the alternative (jettisoning the moral reference point) would require renegotiating the entire treaty architecture.
constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIL SOCIETY COALITION (SCAFFOLD) — NGOs and advocacy groups use UDHR as a temporary coordination mechanism to negotiate state behavior. Coalition sees UDHR not as a permanent framework but as scaffolding supporting the construction of binding treaty obligations (ICCPR, Convention on Torture, etc.). Sunset logic: as binding instruments mature and enforcement mechanisms strengthen (regional courts, treaty-body jurisprudence), the need for aspirational moral guidance diminishes. Organized agents have constrained exit but also agency — they can pivot to binding instruments and away from aspirational rhetoric when efficacy requires it. Sunset estimated at 30–50 years as treaty mechanisms mature and enforce compliance directly.
constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Civilizational perspective that treats UDHR as expressing universal moral truths that are binding independent of state consent. From this view, UDHR articulates natural law: human dignity and fundamental rights exist prior to and independent of any state's ratification decision. The constraint is immutable because it expresses unchangeable moral reality. However, this classification is a false summit: the analytical view naturalizes what is actually a contested institutional arrangement. The 'universality' claim forecloses alternative readings (binding treaty emergence, state veto) and masks the power asymmetry embedded in who gets to define 'universal' human values.
constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(udhr_authority__aspirational_sovereignty_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. This reading defines extractiveness relative to state autonomy — the core claim is that states retain veto power and therefore experience low extraction. However, the metric captures extractiveness from the perspective of this reading's beneficiaries (states). From the perspective of vulnerable populations, extractiveness is much higher (snare perspective: 0.78). The 0.28 value reflects the state-beneficiary perspective embedded in this reading. Suppression (0.32): Moderate. States claiming aspiration-only framing suppress alternative readings (universalism, customary emergence) through doctrinal authority and institutional control of treaty interpretation. But suppression is not complete — alternative readings remain live in scholarship and advocacy. Theater ratio (0.58): Moderate-high and rising. UDHR invocation is increasingly performative: states cite it to legitimize binding obligations (ICCPR, CAT) that derive from other sources; institutions invoke it to justify interpretation expansions technically rooted in binding treaties; advocacy uses aspirational language to negotiate toward binding instruments. Rising trajectory (0.42 → 0.58 over 40-year interval) reflects that UDHR's original coordination function (post-WWII consensus-building) has atrophied while performative invocation has increased.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival gap across its six readings. The sovereign state sees a Rope (pure coordination, state retains control). The vulnerable population sees a Snare (aspiration without remedy, full extraction and suppression). The treaty body sees a Tangled Rope (mixed coordination and institutional extraction). Civil society sees a Scaffold (temporary support for binding-treaty construction with sunset logic). The UDHR institution sees a Piton (degraded ritual maintained through inertia). The universal/civilizational observer risks seeing a Mountain (natural law of human dignity). These are not six interpretations of one classification — they are six structurally distinct constraints generated by reading choices. The false summit (mountain perspective) naturalizes what is actually a contingent institutional reading benefiting state sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to this specific constraint. States retaining treaty veto have low d (0.15–0.25, beneficiaries with arbitrage exit); vulnerable populations have high d (0.90, victims with trapped exit); treaty bodies have moderate-high d (0.55–0.65, institutional extractors with constrained exit); civil society has moderate d (0.40–0.50, organized agents with constrained but agentic exit). The analytical observer at civilizational scope has canonical d for analytical position (0.72) but risks masking the perspectival distribution — treating one position's experienced extractiveness as universal truth.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for UDHR authority is resolved by recognizing that the constraint is not 'which reading is true' but 'which reading are you committing to, and what does that commitment entail?' The aspiration-sovereignty reading is not false — it accurately describes the formal structure of UDHR (aspirational text) and binding treaties (explicit ratification required). But the reading obscures the extraction it enables: vulnerable populations receive moral promise (aspirational language) without remedy (no binding obligation for non-ratifying or non-complying states). The snare reading is equally accurate from the victim perspective. The rope reading is accurate from the beneficiary perspective. The mandatrophy is resolved by mapping the perspectival distribution and recognizing that reading choice determines which experiential perspective becomes naturalized as 'how things are.' This reading's low extractiveness is real for states; the high extractiveness is equally real for vulnerable populations. The constraint's true classification is not a single type but a presheaf over observer positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aspiration_vs_obligation_binding_threshold,
    'At what point does aspirational moral guidance functionally transform into binding obligation? Is there a structural threshold, or is the distinction purely formal/jurisdictional?',
    'Longitudinal case analysis: track state compliance patterns with UDHR-derived binding treaties (ICCPR, CAT, CEDAW) vs non-binding recommendations (UPR, treaty body general comments); assess whether compliance trajectories differ based on binding vs aspirational framing; examine state behavior when UDHR norms conflict with other treaty obligations.',
    'If threshold exists and is structural: aspiration-obligation gap is a real constraint requiring separate story for binding treaty tier. If purely formal: the aspirational reading''s low extractiveness may be understated — aspirational framing disguises binding pressure. If no meaningful distinction: this reading''s rope classification may be oversimplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspiration_vs_obligation_binding_threshold, empirical, 'Structural vs formal distinction between aspirational and binding obligations').

omega_variable(
    state_consent_fiction_vs_reality,
    'Does the requirement for ''state consent'' to binding obligation represent meaningful state agency, or does it naturalize structural coercion (market integration, development conditionality, peer pressure, reputational sanctions)?',
    'Comparative institutional analysis: examine ratification patterns for binding human rights treaties across development levels, geopolitical alignment, and economic dependency; identify correlation between ratification and state capacity, sovereignty constraints, or coercive pressure; assess whether non-ratifying states face material sanctions (trade restrictions, development aid withholding, diplomatic isolation) that functionally eliminate the ''choice'' to refuse.',
    'If state consent is substantively meaningful: this reading''s rope classification holds (states genuinely choose coordination). If consent is largely coerced: the reading masks a snare or tangled_rope structure where states appear to consent but actually face constrained choices. If mixed (varies by state power): different state contexts require different perspectives, suggesting this reading applies only to powerful states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_consent_fiction_vs_reality, empirical, 'Whether state consent to binding obligations is substantively meaningful or structurally coerced').

omega_variable(
    reading_committer_ambiguity,
    'Does this reading''s framing (UDHR as aspirational moral guidance, states retain veto, low extractiveness) reflect genuine structural properties of the UDHR, or does it naturalize the preferences of powerful states that benefit from non-binding moral frameworks?',
    'Committer-axis analysis per Theorem 8: Identify which institutional actors (states, NGOs, treaty bodies, courts) actively maintain this reading vs promote alternatives. Track advocacy and doctrinal evolution in international law scholarship, UN statements, treaty negotiations, and litigation strategy. Assess whether the reading is defended symmetrically across all parties or whether it is contested along power/interest lines. Examine whether weak states, civil society, or treaty bodies push toward binding obligation reading (foreclosing this reading) while powerful states defend sovereignty framing (reinforcing this reading).',
    'If strongly contested along power lines: this reading is a committer-frame choice benefiting state sovereignty coalition, not a discovery of UDHR''s true nature. Classification becomes perspective-dependent: powerful states perceive rope/low extractiveness; vulnerable populations perceive snare/high extractiveness; reading choice reveals whose perspective the constraint is instantiating. If symmetric defense: reading has legitimate structural basis independent of agent interest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Whether this reading represents UDHR''s structural properties or naturalizes state-interest preferences').

omega_variable(
    tribunal_authority_source_ambiguity,
    'When human rights courts (ECHR, IACtHR, African Court) expand UDHR interpretation beyond explicit treaty language, are they exercising delegated state authority or overriding state consent? Does this derive from binding treaties (with explicit ratification) or from aspirational UDHR framing (which lacks consent)?',
    'Doctrinal and institutional analysis: map judicial expansions (living instrument doctrine, dynamic interpretation, rights expansion into treaty silence) to their formal authority source (treaty clause, general principles, UDHR reference); assess state response (acceptance, modification, withdrawal, non-compliance); examine whether states ratified binding instruments expecting narrow interpretation later expanded by courts. Track state exit costs if they challenge expanded interpretation.',
    'If courts derive authority from binding treaties alone: aspiration-obligation distinction holds and this reading is structurally accurate. If courts invoke UDHR aspirational framing to justify expansion: aspirational reading disguises actual binding imposition (tangled_rope or snare logic — states consented to narrow treaties but received expanded obligations). If asymmetric (powerful states ignore court expansion; weak states cannot): perspectival gap reveals reading masks power asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribunal_authority_source_ambiguity, empirical, 'Authority source for human rights court interpretation expansions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_asp_theater_t0, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(udhr_asp_theater_t20, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(udhr_asp_theater_t40, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(udhr_asp_extract_t0, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(udhr_asp_extract_t20, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(udhr_asp_extract_t40, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__customary_emergence_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, international_treaty_consent_mechanism).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, human_rights_court_authority_expansion).

% DUAL FORMULATION NOTE:
% UDHR authority decomposes into three structurally distinct constraints corresponding to three readings of the kernel: (1) aspirational_sovereignty_reading (this story) — low extractiveness on states, high on vulnerable populations, coordinated through moral framing; (2) binding_universalism_reading — high extractiveness uniformly, universal obligation overrides state veto; (3) customary_emergence_reading — extractiveness depends on customary law evidence, moderate for all actors through implicit obligation. Each reading has different ε, different beneficiary/victim structure, different temporal trajectory. The three stories form a constraint family linked by network.affects_constraints. The reading choice reflects and reinforces institutional power distributions — state-centered international law doctrine defends reading (1); human rights advocacy defends (2)/(3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__aspirational_sovereignty_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
