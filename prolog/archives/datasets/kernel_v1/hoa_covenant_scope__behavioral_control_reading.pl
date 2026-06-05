% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Behavioral Control Reading: Aesthetic Uniformity as Extraction
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   HOA covenants exist in contested readings. The behavioral_control_reading
 *   instantiates one specific interpretation: covenants function as
 *   mechanisms for enforcing aesthetic uniformity and lifestyle conformity as
 *   a strategy for maximizing property values for the conformist majority.
 *   Under this reading, the covenant is a snare: it benefits board-aligned
 *   homeowners and conformists (low extraction experienced), traps
 *   nonconformists and marginal-aesthetic dwellers (high extraction,
 *   suppression, and no exit option), and uses performative justification
 *   (property value protection) to legitimize subjective lifestyle
 *   enforcement. This is distinct from the coordination_reading (covenants
 *   solve genuine collective action problems around neighborhood quality) and
 *   the extraction_reading (covenants are pure exploitative tools with no
 *   coordination function). The behavioral_control reading occupies the
 *   middle ground: it acknowledges that some coordination occurs (property
 *   values do respond to aesthetic uniformity) while asserting that the
 *   mechanism has crossed into extractive behavioral control. The kernel is
 *   the covenant document itself — the same legal text that coordinates
 *   property maintenance under one reading becomes an apparatus for
 *   conformity enforcement under this reading. The reading is live and
 *   contested within communities; some HOAs embrace behavioral control
 *   explicitly while others resist it as exceeding legitimate covenant scope.
 *
 * KEY AGENTS:
 *   - Nonconformist Homeowners: Primary victims (powerless/trapped) — bear extraction cost of lifestyle restrictions, fines, and subjective violation threats; cannot exit without liquidating illiquid asset at penalty
 *   - Board-Aligned Majority: Primary beneficiaries (institutional/arbitrage) — experience coordination benefit (aesthetic uniformity, property value maintenance) with low personal extraction cost (rules align with preferences)
 *   - Property Rights Coalition: Organized opposition (organized/constrained) — legal advocates, residents' associations; face coordination costs but have leverage to contest scope
 *   - Future Residents: Generational victims (powerless/trapped) — inherit covenant obligations without consent; facing multi-generational enforcement lock
 *   - HOA Board: Institutional enforcer (institutional/arbitrage) — derives authority and enforcement capacity from covenant kernel; benefits from enforcement (legitimacy, control, board alignment)
 *   - Real Estate Market: Structural beneficiary (institutional/arbitrage) — covenant uniformity raises median property values during appreciation phases; creates liquidity penalties for nonconforming properties during downturns
 *   - Analytical Observer: Sees false summit risk (analytical/analytical) — risks naturalizing conformity enforcement as property rights necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.48).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.68).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Behavioral Control Reading: Aesthetic Uniformity as Extraction").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f').
narrative_ontology:cs_kernel_codification('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', fixed_text).
narrative_ontology:cs_authority_grounding('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', extraction).
narrative_ontology:cs_interpretation_layer_present('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f').
narrative_ontology:cs_reading_relation('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', foundational, aesthetic_preference_majoritarian_naturalization).
narrative_ontology:cs_axiom_status(aesthetic_preference_majoritarian_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', aesthetic_preference_majoritarian_naturalization, empirically_contingent).
narrative_ontology:cs_axiom('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', secondary, consent_through_purchase_presumption).
narrative_ontology:cs_axiom_status(consent_through_purchase_presumption, holdable).
narrative_ontology:cs_axiom_grounding('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', consent_through_purchase_presumption, deontological).
narrative_ontology:cs_reference_frame('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', property_rights_autonomy_framework).
narrative_ontology:cs_drift_state('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', contemporary_enforcement_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9a9ab5e8-f52b-45ce-9e5d-05dcddcf827f', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, property_value_maximizers).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformists).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_dwellers).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, speech_constrained_residents).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, eccentric_expression_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NONCONFORMIST HOMEOWNER (SNARE) — Trapped by illiquid asset (home equity requires sale to exit), identity-constrained by residential stakes (family, schools, community roots), subject to escalating fines and liens for subjective aesthetic violations (yard color, landscaping style, flag presence, sign content). Extraction mechanism is enforcement threat + illiquidity + identity lock. No coordination benefit; pure coercive compliance.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BOARD-ALIGNED MAJORITY (ROPE) — Experiences the constraint as pure coordination: uniform aesthetics maintain property values (coordination benefit) and enforces norms preferred by the majority. The beneficiary perceives low extraction cost (rules align with their preferences) and high coordination benefit (value maintenance, neighborhood aesthetic control). Exit option is arbitrage — they can leave without penalty if they disagree, but don't, because the constraint benefits them.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: PROPERTY RIGHTS COALITION (TANGLED ROPE) — Organized opposition to covenant scope expansion sees both genuine coordination (baseline neighborhood quality standards) and asymmetric extraction (enforcement scope creeping into subjective lifestyle control). The coalition has exit leverage (legal challenge, legislative advocacy) but faces coordination costs and weak enforcement of anti-covenant protections. Moderate effective extraction because organized agents have agency.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FUTURE RESIDENTS / GENERATIONAL (SNARE) — Covenant runs with the land; future generations inherit behavioral obligations they did not consent to. Each cohort that purchases enters trapped within covenant scope without realistic renegotiation. The constraint appears immutable from a generational horizon because exit (sale) permanently exits from property ownership within the region, not just from the covenant.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: CONFLICTED MAJORITY MEMBER (TANGLED ROPE) — Nominally aligned with board but experiences private costs (surveillance, conformity pressure, fear of violation). Derives coordination benefit (property values, neighborhood predictability) and extraction cost (loss of autonomy in personal expression, aesthetic preferences). Exit is costly (selling into down market) and incomplete (new neighborhood likely has own covenant). Mixed classification reflects genuine internal conflict.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: COVENANT INSTITUTION (PITON) — At the institutional/civilizational level, covenants are residual legal instruments from an earlier era (restrictive covenants, racial and religious exclusions). The behavioral control reading repurposes them for modern aesthetics and lifestyle enforcement. The institution persists through legal inertia (covenants run with land, hard to amend) and performative justification (property value protection) even as their primary extraction mechanism has shifted. Theater_ratio is moderate (some genuine coordination, some performance) because the institution still claims property-value justification even when enforcement targets subjective lifestyle choices.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a legal-naturalist position grounded in property rights theory, covenants represent immutable contracted-for restrictions on use: if buyers knowingly accepted covenant terms, the behavioral control is not extraction but legitimate contractual enforcement. This perspective naturalizes consent and treats covenant scope as analytically inseparable from property rights themselves. However, the base metrics reveal this as a false summit: information asymmetry (fine print), illiquidity of exit, and scope expansion beyond signed terms contradict the clean consent narrative.
constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hoa_covenant_scope__behavioral_control_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, TR),
    TR >= 0.70.

:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The behavioral_control reading asserts that extractive mechanisms operate through: (1) enforced conformity generating utility for the majority at the cost of individual expression for minorities, (2) valuation of uniformity as artificial market signal (not intrinsic to property utility), and (3) scope expansion over time (enforcement creep from objective structural standards to subjective lifestyle judgments). The measurement trajectory shows extractiveness rising from 0.30 (early covenant, objective standards) to 0.48 (mature covenant, subjective enforcement). The 0.48 value reflects that genuine property-value coordination exists (not pure extraction) but is asymmetrically distributed — majority benefits, minorities bear cost. Suppression (0.68): High. Mechanisms include: illiquidity of residential exit (home is largest asset), identity lock (family, schools, community roots anchor residents), fine and lien threats (financial enforcement), and surveillance norm (neighbors monitoring compliance). The trajectory shows suppression rising from 0.55 to 0.68 as enforcement mechanisms mature and social surveillance becomes normalized. Theater ratio (0.55): Moderate. The constraint performs value protection (genuine function) but increasingly performs conformity enforcement as spectacle (yard inspections, flag enforcement, color audits). The 0.55 value reflects mixed genuine and performative content — property values do respond to uniformity, but enforcement scope expands beyond what property protection requires.
 *
 * PERSPECTIVAL GAP:
 *   The behavioral_control reading produces maximum perspectival gap. The board-aligned majority sees rope (coordination benefit, low personal extraction). The nonconformist sees snare (pure extraction, no exit). The organized coalition sees tangled rope (acknowledges coordination but contests asymmetric extraction). The institutional covenant itself sees piton (performing its function through inertia, not dynamic mechanism). The natural law observer risks seeing mountain (covenants as immutable property rights) — the false summit detector should flag this. The reading's analytical power is that it demonstrates how the same legal structure (the covenant document) can be legitimately read as solving different problems: under coordination_reading, it solves the prisoner's dilemma of neighborhood quality (Rope from all perspectives). Under behavioral_control_reading, it extracts conformity value from minorities to benefit majorities (Snare from victims, Rope from beneficiaries). Under extraction_reading, it would be pure Snare from all perspectives. The perspectival gap reveals that the choice of reading is not empirical but institutional — which reading dominates depends on whether the covenant's primary enforcement target is objective standards (structural safety, property maintenance) or subjective preferences (aesthetic style, lifestyle conformity).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. The board-aligned majority are beneficiaries with arbitrage exit (they can leave if rules change; don't because they prefer them) — this drives d toward 0.15, producing low or negative chi (they experience the constraint as beneficial, not extractive). Nonconformists are victims with trapped exit (illiquid asset, identity lock) — this drives d toward 0.95, producing high chi and high f(d), strong experienced extraction. The organized coalition has constrained exit (costly legal challenge, legislative advocacy) — this drives d toward 0.60, producing moderate chi. The future generations face trapped exit at generational horizon — d toward 0.95 at generational time, producing snare classification. The institutional covenant observer derives d from the power atom institutional (canonical 0.00, producing negative chi) — the institution experiences the constraint as its own legitimate authority. The analytical observer derives d from analyst canonical (0.73, producing high f(d)) but must account for the false summit risk: the mountain perspective's d should shift upward if the natural law framing is unmasked as beneficiary-serving naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The behavioral_control reading resolves mandatrophy by specifying which beneficiary/victim structure drives the snare classification. The constraint is not ambiguously snare vs rope — it's snare-from-the-nonconformist-perspective and rope-from-the-beneficiary-perspective. The same base metrics produce different classifications because the structural positions are different. The mandatrophy is resolved by declaring that the reading is about asymmetric extraction: the constraint coordinates aesthetic uniformity (genuine coordination function) but distributes its benefits asymmetrically (majority captures value, minorities bear conformity cost). This is a tangled_rope structure from a system view but appears as snare from the victim's perspective. The behavioral_control_reading asserts that the asymmetry has become the primary function: extracting conformity from minorities is not a side effect of coordination but the central mechanism. Under this reading, the measuring agent should ask: is the covenant's enforcement scope determined by property-value coordination needs (objective standards) or by majority aesthetic preferences (subjective control)? If the answer is majority preferences, the reading is confirmed and snare classification is appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjective_aesthetic_standard_drift,
    'Does the covenant enforce objective property maintenance standards (structural integrity, public safety) or subjective aesthetic preferences (color palette, landscaping style, personal expression)?',
    'Content analysis of enforcement actions: ratio of objective (structural safety) vs subjective (aesthetic, lifestyle) violations cited; review of board meeting minutes and variance denial records; correlation between enforcement intensity and resident aesthetic divergence from board preferences',
    'If predominantly objective: snare classification weakens to tangled_rope (genuine coordination with some extraction overhead). If predominantly subjective: snare classification is reinforced (extraction mechanism is normalization of majority taste as natural law). If mixed with no objective boundary: extractiveness increases to 0.60+ (scope creep without natural limits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjective_aesthetic_standard_drift, empirical, 'Covenant enforcement: objective property standards vs subjective aesthetic control').

omega_variable(
    consent_information_asymmetry,
    'Do homebuyers at purchase time have clear, accessible knowledge of covenant scope, enforcement history, and likelihood of violation across common residential choices (yard color, landscaping, signage, flag display)?',
    'Survey of buyer disclosure practices; analysis of title documentation clarity; comparison of covenant text complexity vs typical buyer comprehension; longitudinal tracking of surprise enforcement actions against first-time violators',
    'If information is clear and accessible: consent narrative strengthens (buyers knowingly accepted terms) and extraction reading weakens. If systematically opaque: false consent inference (buyers cannot opt out informed) and extraction reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_information_asymmetry, empirical, 'Information asymmetry in covenant disclosure and buyer consent').

omega_variable(
    alternative_value_protection_mechanisms,
    'Are there effective alternatives to behavioral covenant enforcement for protecting property values (municipal zoning, neighborhood design standards, peer-norm enforcement)?',
    'Comparative analysis of neighborhood property value stability across jurisdictions with strict covenants vs communities relying on zoning alone vs peer-norm enforcement; regression analysis of value protection efficacy controlling for location, market conditions, and demographic shifts',
    'If strong alternatives exist: behavioral covenants appear as choice mechanism (scaffold reading gains plausibility). If no effective alternatives: behavioral enforcement appears necessary (tangled_rope coordination cost justified). If substitutes underperform: extractive covenant control is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_value_protection_mechanisms, empirical, 'Necessity and efficacy of behavioral covenant enforcement vs alternatives').

omega_variable(
    scope_expansion_mechanism,
    'Does covenant scope expand over time through board reinterpretation, or does it remain fixed at original signing?',
    'Historical analysis of covenant enforcement scope: comparison of enforcement actions in year 1-5 vs year 15-20 of covenant history; review of board minutes for scope reinterpretation language; analysis of variance petition decisions as scope signal',
    'If stable scope: original consent narrative holds (buyers agreed to this and no creep occurred). If expanding scope: scope-creep mechanism confirmed (extraction increases as enforcement perimeter widens beyond original consent). If expansion is formalized through amendment: governance mechanism visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_expansion_mechanism, empirical, 'Covenant scope expansion or stability over time').

omega_variable(
    reading_kernel_contestation,
    'Which reading of the covenant kernel do residents and board members themselves espouse: behavioral control as legitimate coordination, as necessary value protection, or as extractive conformity enforcement?',
    'Survey of residents and board members: explicit vignettes describing enforcement actions with questions about whether covenant is serving coordination/value protection/conformity control; analysis of variance appeal justifications; examination of community discourse (HOA newsletter, resident forum, meeting minutes) for reading adoption language',
    'If residents/board explicitly endorse behavioral_control reading: coordination function claim becomes transparent as preference-laundering (extraction acknowledged). If residents dispute the reading: contestation of the kernel is live (coexists_with relation holds). If residents are unaware of scope expansion: information asymmetry mechanism confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contestation, empirical, 'Which reading of covenant kernel do stakeholders explicitly adopt').

omega_variable(
    false_summit_covenant_naturalization,
    'Is the covenant''s behavioral control legitimated as a natural property law (immutable contractual obligation) or as a contingent institutional choice?',
    'Discourse analysis: examine how covenants are presented in real estate marketing, legal documents, board communications, and buyer counseling. Assess whether covenant scope is framed as ''natural property rights'' vs ''community standards'' vs ''extractive conformity.'' Test against counterfactual: if covenant scope were narrowed to objective standards only, would buyers experience it as property rights violation or as policy adjustment?',
    'If naturalized as law: false summit diagnosis confirmed (beneficiaries treat behavioral control as inevitable). If treated as contingent policy: coordination/extraction distinction remains contestable. If buyers would accept narrowing: consent narrative fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_covenant_naturalization, conceptual, 'Whether behavioral covenant control is naturalized as immutable law or recognized as contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_behavioral_tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hoa_behavioral_tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(hoa_behavioral_tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(hoa_behavioral_be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hoa_behavioral_be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(hoa_behavioral_be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hoa_behavioral_su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hoa_behavioral_su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(hoa_behavioral_su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, restrictive_covenant_racial_legacy).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, property_rights_consent_narrative).

% DUAL FORMULATION NOTE:
% The HOA covenant constraint family decomposes into three structurally distinct readings of the same kernel (the covenant document). Each reading has different ε, beneficiary/victim structure, and classification profile. The behavioral_control_reading (this file) emphasizes lifestyle enforcement asymmetry; the coordination_reading emphasizes property-value coordination; the extraction_reading emphasizes coercive mechanisms with minimal coordination. Sibling readings coexist in live dispute within communities. Constraint family is linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
