% ============================================================================
% CONSTRAINT STORY: westminster_evolution__british_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westminster_evolution__british_constitution, []).

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
 *   constraint_id: westminster_evolution__british_constitution
 *   human_readable: British Constitutional Settlement: Unwritten Sovereignty Tempered by Convention
 *   domain: political/legal
 *
 * SUMMARY:
 *   The British constitutional settlement is the Western world's original
 *   unwritten constitution — a framework that rests on parliamentary
 *   sovereignty tempered by evolved convention and common law rather than a
 *   codified text. This reading instantiates ONE perspective on a contested
 *   kernel: what Westminster governance IS and whether its unwritten
 *   character is a feature (flexibility, organic evolution, responsiveness to
 *   circumstance) or a vulnerability (no entrenched rights, extraction masked
 *   by convention, suppression of alternatives). The constraint story models
 *   the British constitution as a tangled rope — a genuine coordination
 *   mechanism for elite governance alongside asymmetric extraction of those
 *   who lack voice in its evolution and have no entrenched protection against
 *   parliamentary revocation. The unwritten character enables flexibility for
 *   those who control Parliament but denies rights to those who do not.
 *   Suppression operates through convention (what is not done is not done)
 *   rather than through law, making the suppression mechanism partially
 *   self-enforcing — subjects internalize the boundaries that convention
 *   defines. The theater ratio (0.58) reflects that constitutional convention
 *   is substantially performative: the rituals of parliamentary procedure,
 *   the Crown's ceremonial role, and the unspoken understandings persist
 *   through shared assumption rather than legal enforcement. The sibling
 *   reading (westminster_export_constitutions) sees the same power structure
 *   codified and exported to postcolonial societies, revealing the tension
 *   between the claim of organic evolution (a feature of the British context)
 *   and the claim of universal applicability (implied when Westminster
 *   governance is transplanted).
 *
 * KEY AGENTS:
 *   - Parliament and the Governing Class (institutional/arbitrage): Primary beneficiary — controls the unwritten framework, determines what conventions mean, can revise the constitution by simple majority, benefits from flexibility without legal constraint
 *   - Subjects Without Entrenched Rights (powerless/trapped): Primary victim — possess no codified rights that override parliamentary action, face revocation risk, cannot appeal to a written standard that would constrain Parliament
 *   - Governed Elite Outside Parliament (powerful/constrained): Secondary actors — judges, lords, Crown officials; benefit from coordination with Parliament but face extraction through lack of entrenchment; powerful exit options exist but constrained by embedding
 *   - Merchant and Professional Classes (moderate/constrained): Secondary actors — benefit from constitutional stability and predictable property enforcement; face extraction through revocable privileges; constrained exit through capital mobility and professional reputation costs
 *   - Convention-Based Enforcement Apparatus (institutional/arbitrage): Institutional mechanism — sustains the unwritten constitution through rituals and shared understandings; increasingly theatrical as its original function (preventing executive abuse) has shifted to other mechanisms
 *   - Analytical Observer (analytical/analytical): Civilizational perspective — risks naturalizing a contingent institutional arrangement as an irreducible feature of parliamentary governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westminster_evolution__british_constitution, 0.28).
domain_priors:suppression_score(westminster_evolution__british_constitution, 0.42).
domain_priors:theater_ratio(westminster_evolution__british_constitution, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westminster_evolution__british_constitution, extractiveness, 0.28).
narrative_ontology:constraint_metric(westminster_evolution__british_constitution, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(westminster_evolution__british_constitution, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westminster_evolution__british_constitution, tangled_rope).
narrative_ontology:human_readable(westminster_evolution__british_constitution, "British Constitutional Settlement: Unwritten Sovereignty Tempered by Convention").
narrative_ontology:topic_domain(westminster_evolution__british_constitution, "political/legal").

domain_priors:requires_active_enforcement(westminster_evolution__british_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westminster_evolution__british_constitution, '9c578db3-120c-41fd-a51e-293c44bf5ae4').
narrative_ontology:cs_kernel_codification('9c578db3-120c-41fd-a51e-293c44bf5ae4', implicit).
narrative_ontology:cs_authority_grounding('9c578db3-120c-41fd-a51e-293c44bf5ae4', lineage).
narrative_ontology:cs_interpretation_layer_present('9c578db3-120c-41fd-a51e-293c44bf5ae4').
narrative_ontology:cs_reading_relation('9c578db3-120c-41fd-a51e-293c44bf5ae4', westminster_evolution__westminster_export_constitutions, coexists_with).
narrative_ontology:cs_axiom('9c578db3-120c-41fd-a51e-293c44bf5ae4', foundational, unwritten_flexibility_enables_governance).
narrative_ontology:cs_axiom_status(unwritten_flexibility_enables_governance, holdable).
narrative_ontology:cs_axiom_grounding('9c578db3-120c-41fd-a51e-293c44bf5ae4', unwritten_flexibility_enables_governance, conventional).
narrative_ontology:cs_axiom('9c578db3-120c-41fd-a51e-293c44bf5ae4', foundational, parliamentary_sovereignty_supremacy).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9c578db3-120c-41fd-a51e-293c44bf5ae4', parliamentary_sovereignty_supremacy, deontological).
narrative_ontology:cs_reference_frame('9c578db3-120c-41fd-a51e-293c44bf5ae4', evolved_parliamentary_settlement).
narrative_ontology:cs_drift_state('9c578db3-120c-41fd-a51e-293c44bf5ae4', contemporary_executive_penetration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c578db3-120c-41fd-a51e-293c44bf5ae4', '').
narrative_ontology:cs_kernel_id(westminster_evolution__british_constitution, westminster_evolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westminster_evolution__british_constitution, parliament_governing_class).
narrative_ontology:constraint_victim(westminster_evolution__british_constitution, subjects_lacking_entrenched_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT WITHOUT ENTRENCHED RIGHTS (SNARE) — Citizens cannot exit parliamentary sovereignty or appeal to a codified standard that would constrain Parliament. Rights exist only as long as Parliament does not revoke them by simple majority. The suppression is total: there is no legal recourse to a written constitution that would override Parliament. The subject is trapped within a framework that grants rights by grace of the sovereign legislature, not by entrenched claim.
constraint_indexing:constraint_classification(westminster_evolution__british_constitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENT AND GOVERNING CLASS (ROPE) — Benefits from the unwritten constitution's flexibility and the convention-based suppression that prevents challenges to parliamentary supremacy. Experiences the constraint as pure coordination: the evolved settlement of conventions enables parliamentary governance without the friction of written constitutional amendment. The arbitrage benefit is institutional — Parliament's power is both constrained by convention (enabling predictable governance) and unlimited by written law (enabling response to circumstance). Net beneficiary with significant exit option.
constraint_indexing:constraint_classification(westminster_evolution__british_constitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNED ELITE OUTSIDE PARLIAMENT (TANGLED ROPE) — Lords, judges, Crown officials, and institutional interests have genuine coordination function with Parliament (judicial independence de facto, peerage convention) but also face extraction: their privileges rest on parliamentary acquiescence, not entrenched right. Powerful exit options exist (capital mobility, institutional exit to the City or the professions) but constrained by reputation costs and institutional embedding. Experience is mixed: genuine coordination of elite governance alongside asymmetric extraction (Parliament's right to revoke any privilege by simple act).
constraint_indexing:constraint_classification(westminster_evolution__british_constitution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MERCHANT AND PROFESSIONAL CLASSES (TANGLED ROPE) — Benefit from the constitutional settlement's stability and the convention-based predictability that enables commerce and professional practice. Also face extraction: property rights, guild privileges, and professional licensing rest entirely on parliamentary goodwill with no entrenched defense. Moderately constrained exit (can emigrate, can shift capital, but costly). Genuine coordination function (the settlement enables reliable property enforcement, contract law) alongside suppression (no written guarantee against future revocation).
constraint_indexing:constraint_classification(westminster_evolution__british_constitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONVENTION-BASED ENFORCEMENT APPARATUS (PITON) — The machinery of constitutional convention — the rituals of parliamentary procedure, the Crown's ceremonial role, the unspoken understandings that structure governance — is substantially theatrical. These conventions persist through institutional inertia and shared assumption rather than active enforcement. Most conventions are never breached because breach is unthinkable within the governing frame, not because breach carries legal penalty. The apparatus is degraded in that it has lost its primary function (it once prevented arbitrary executive power, but now it performs that role only theatrically as actual power has shifted to the executive within the parliamentary frame). It persists because the cost of replacing it with explicit written rules exceeds the cost of maintaining the theatrical performance.
constraint_indexing:constraint_classification(westminster_evolution__british_constitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the unwritten constitutional settlement appears as an emergent natural law of parliamentary governance — a product of organic evolution rather than design, stabilized by repeated practice, and resistant to deliberate codification. The view sees parliamentary sovereignty as inherent to the Westminster system and suppression by convention as an irreducible feature of how unwritten constitutions function. However, this perspective risks naturalizing a contingent institutional arrangement. The structural data reveals that the settlement distributes extraction to those without voice in its evolution and grants benefits to those fluent in its usages. The 'natural' appearance masks the beneficiary group.
constraint_indexing:constraint_classification(westminster_evolution__british_constitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westminster_evolution__british_constitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westminster_evolution__british_constitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westminster_evolution__british_constitution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(westminster_evolution__british_constitution, TR),
    TR >= 0.70.

:- end_tests(westminster_evolution__british_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The British constitution constrains parliamentary sovereignty through evolved convention and common law, creating a coordination mechanism for stable elite governance. However, the lack of written entrenchment means that any rights held by subjects exist only at parliamentary pleasure. The extractiveness is not extreme because: (a) conventions do function as genuine constraints in practice, (b) the governing class is bound by the same conventions, and (c) the settlement has evolved through negotiation rather than imposition. But extractiveness is not negligible because: (a) subjects have no legal recourse against parliamentary revocation, (b) the beneficiary group (Parliament and those fluent in its usages) can unilaterally redefine the settlement within constitutional boundaries, and (c) conventions suppress explicit alternatives (written entrenchment, codified rights). The measurement trajectory (0.20 → 0.28) reflects gradual erosion of convention's binding force and increased executive penetration of constitutional limits in the modern era. Suppression (0.42): Moderate. Suppression operates through convention rather than law — the mechanism is what is not done rather than what is forbidden. This creates internalized suppression: subjects accept boundaries they have internalized as natural or inevitable. Alternative framings (written constitutionalism, entrenched rights) are suppressed not by legal prohibition but by the governing class's epistemic monopoly on how governance should work. Suppression is moderate rather than high because: (a) conventions are genuinely constraining and not entirely performative, (b) the suppression of alternatives is cultural/epistemic rather than coercive, and (c) challengers can still articulate alternatives, though at reputational cost. Theater ratio (0.58): Moderate-high. Constitutional convention is substantially performative. The rituals of parliamentary procedure, the Crown's ceremonial role, the unwritten understandings that structure governance persist through shared expectation and institutional inertia. Most conventions are never breached not because breach carries legal penalty but because breach is unthinkable within the governing frame. The theater has increased over the interval as the regulatory function of convention has declined and procedural ritual has become less tightly linked to actual power constraints.
 *
 * PERSPECTIVAL GAP:
 *   The British constitutional reading exhibits sharp perspectival gaps across the power axis. Parliament sees pure coordination (Rope) — the flexible framework that enables responsive governance. The governing elite outside Parliament see tangled rope (mixed coordination of judicial independence, peerage convention, professional licensing alongside extraction through revocability). Merchant and professional classes see tangled rope at a moderate power level (genuine commerce-enabling function alongside suppressed rights). Subjects without entrenched rights see snare (no exit, no legal recourse, extraction masked by conventions they internalize as natural). The civilizational analytical perspective risks seeing mountain (natural law of parliamentary evolution) but the structural data reveals this as a false summit: the settlement is beneficiary to a specific group (Parliament and those fluent in conventions) and victim to those without entrenchment. The core perspectival tension is between those who control the unwritten framework (beneficiaries) and those who must accept its boundaries without legal recourse (victims).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural position of each agent — their power level, exit options, and relationship to the extraction flow. Parliament and the governing class occupy d ≈ 0.15 (arbitrage exit, beneficiary status, institutional power) — they experience low effective extraction because they define and control the constraint. Subjects without entrenched rights occupy d ≈ 0.92 (trapped exit, victim status, powerless) — they experience maximum extraction because they have no legal defense against parliamentary action and no alternative framework. The governed elite outside Parliament occupy d ≈ 0.55 (constrained exit, mixed beneficiary/victim status, powerful) — they benefit from convention-based coordination but face extraction through revocability. The merchant and professional classes occupy d ≈ 0.65 (constrained exit, victim status in terms of rights, moderate power) — they are constrained by mobility costs and embedding but have resources to navigate the system. The analytical observer occupies d ≈ 0.72 (analytical exit, observer status, analytical power) — they see the full structure but risk naturalizing it as inevitable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_as_genuine_constraint_vs_performance,
    'Do constitutional conventions function as genuine constraints on parliamentary action or primarily as performative scripts that persist through social expectation?',
    'Historical analysis of convention breaches: when conventions have been explicitly violated (prorogation, dissolution precedent changes), what enforcement mechanism operated? Did legal consequences follow, or only reputational/electoral consequences? Comparative analysis of written vs unwritten constitutional constraints on the same powers.',
    'If genuine constraints: suppression and extractiveness are lower than estimated; the constraint approaches pure coordination (Rope from more perspectives). If performative: suppression is internalized rather than structural; the constraint is more extractive than the metrics suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convention_as_genuine_constraint_vs_performance, empirical, 'Whether conventions enforce as law or as social expectation').

omega_variable(
    entrenched_rights_enforceability_counterfactual,
    'Would a written entrenched rights framework (e.g., a codified bill of rights with supermajority amendment requirement) actually constrain Parliament''s power to extract, or would Parliament find it as revocable as conventions?',
    'Comparative constitutional analysis: study regimes with supermajority requirements for rights amendment (Australia, Canada, India, South Africa) and examine how often rights have been suspended or the supermajority bypassed. Historical analysis of the 1948 UK debates on codification: what arguments were offered for rejecting written entrenchment?',
    'If entrenchment would genuinely constrain: the current settlement''s extractiveness is higher than the metrics suggest — suppression by convention is weaker than suppression by entrenched law. The constraint should reclassify toward snare. If Parliament would treat entrenchment as merely conventional (as it treats parliamentary procedure): the reading''s core premise (unwritten flexibility as superior to written constraint) is validated, but the extraction mechanism is revealed as internalized rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenched_rights_enforceability_counterfactual, conceptual, 'Whether written entrenchment would actually constrain parliamentary extraction').

omega_variable(
    knowledge_asymmetry_as_suppression_mechanism,
    'Is the suppression of alternatives (entrenchment, written constitutionalism) enforced by structural barriers or by the governing class''s epistemic monopoly on ''how things are done''?',
    'Survey and interview data from UK political classes: can non-members of the governing elite articulate the unwritten constitution and its conventions? What fraction of the population could name a single constitutional convention? Comparison with public knowledge of written constitutions in other democracies. Analysis of constitutional education in UK schools vs. other democracies.',
    'If enforced by epistemic monopoly: suppression is internalized/cultural rather than structural; the constraint is more dependent on the identity-locked exit option than the metrics suggest. The identity-lock is cognitive capture within a governing tradition. If structural barriers exist: suppression is real and externalizable; entrenched rights could genuinely change the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_asymmetry_as_suppression_mechanism, empirical, 'Whether suppression of alternatives is structural or epistemic').

omega_variable(
    sibling_reading_empirical_trigger,
    'When Westminster governance is exported to postcolonial societies, does the written codification of unwritten British practice reveal the practice as contingent on British cultural embedding, or does the codification successfully instantiate the same power structure in new contexts?',
    'Comparative case analysis: postcolonial Westminster exports (India, Australia, Canada, Kenya, Jamaica) and their success/failure in replicating British parliamentary stability. Track how written constitutions exported from Britain diverge from British practice and what this reveals about whether the unwritten settlement was culture-contingent or power-structural.',
    'If export succeeds structurally: the unwritten settlement is a power structure transferable to any context, and codification merely makes explicit what was always implicit. The British reading''s core claim (unwritten flexibility is superior) is undermined. If export creates different power structures: the unwritten settlement is culture-contingent and its effectiveness depends on shared Anglo epistemic background. The British reading''s claim is validated but requires cultural reproduction, not merely institutional stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_trigger, empirical, 'Whether Westminster governance is culturally contingent or universally structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westminster_evolution__british_constitution, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westminster_evolution__british_constitution, theater_ratio, 0, 0.45).
narrative_ontology:measurement(west_tr_t200, westminster_evolution__british_constitution, theater_ratio, 200, 0.52).
narrative_ontology:measurement(west_tr_t350, westminster_evolution__british_constitution, theater_ratio, 350, 0.58).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westminster_evolution__british_constitution, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(west_be_t200, westminster_evolution__british_constitution, base_extractiveness, 200, 0.24).
narrative_ontology:measurement(west_be_t350, westminster_evolution__british_constitution, base_extractiveness, 350, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westminster_evolution__british_constitution, enforcement_mechanism).
narrative_ontology:affects_constraint(westminster_evolution__british_constitution, westminster_evolution__westminster_export_constitutions).

% DUAL FORMULATION NOTE:
% The westminster_evolution kernel is modeled as two constraint stories with different beneficiary/victim structures and different extractiveness values. The british_constitution reading (this file) models the unwritten settlement as operating within the British context with suppression by convention and beneficiary status for Parliament. The westminster_export_constitutions reading models the same power structure codified and transplanted to postcolonial societies, where the written form reveals and potentially constrains the extraction differently. Both stories link to each other via network.affects_constraints because the empirical status of one reading (does Westminster export succeed?) provides evidence about the other reading (is the unwritten settlement culture-contingent or universally structural?).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
