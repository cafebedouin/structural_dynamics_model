% ============================================================================
% CONSTRAINT STORY: citizenship_clause__allegiance_qualified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citizenship_clause__allegiance_qualified_reading, []).

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
 *   constraint_id: citizenship_clause__allegiance_qualified_reading
 *   human_readable: Citizenship Clause — Allegiance-Qualified Reading
 *   domain: constitutional/doctrinal/citizenship
 *
 * SUMMARY:
 *   The allegiance-qualified reading interprets the Fourteenth Amendment's
 *   citizenship clause as conditioning birthright citizenship on more than
 *   geographic presence — it requires allegiance to the United States. This
 *   reading suppresses pure territorialism and makes membership contingent on
 *   a political tie. Children born to foreign nationals owing primary
 *   allegiance elsewhere fall outside the clause's grant, regardless of birth
 *   location. This is ONE of three structurally distinct readings of the same
 *   constitutional kernel (the citizenship clause). The reading instantiates
 *   a membership-by-consent doctrine where citizenship is a contract
 *   requiring political allegiance, not an automatic property of birth within
 *   the territory. The constraint exhibits the full range of classification
 *   from different perspectives: victims (children excluded from birthright)
 *   experience snare dynamics; beneficiaries (the polity screening for
 *   allegiance) experience rope coordination; organized advocates experience
 *   tangled-rope hybridity; the naturalization pipeline offers
 *   temporary-support structure; the doctrine machinery itself appears as a
 *   degraded piton persisting through legal inertia; and the analytical
 *   observer risks naturalizing the allegiance requirement as an unchangeable
 *   feature of political order (false summit).
 *
 * KEY AGENTS:
 *   - Children born to foreign allegiance: Primary victims (powerless/trapped) — excluded from birthright citizenship without parental allegiance shift
 *   - Membership-by-consent doctrine: Primary beneficiary (institutional/arbitrage) — allegiance requirement enables screening for committed members
 *   - Allegiance-based polity: Institutional beneficiary (institutional/arbitrage) — experiences allegiance doctrine as coordination mechanism, not extraction
 *   - Temporarily present residents: Secondary victims (moderate/constrained) — citizenship delayed or conditioned on allegiance demonstration
 *   - Stateless cohort: Generational victim (powerless/trapped) — long-term structural effect of allegiance doctrine creates statelessness across generations
 *   - Immigration rights advocates: Organized opponents (organized/constrained) — contest allegiance doctrine, advocate for territorial birthright expansion
 *   - Naturalization pipeline: Institutional mediator (institutional/arbitrage) — offers time-limited exit path through structured allegiance demonstration
 *   - Constitutional doctrine machinery: Institutional maintainer (institutional/arbitrage) — preserves allegiance framing through doctrinal authority despite weak functional necessity
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing political allegiance as unchangeable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citizenship_clause__allegiance_qualified_reading, 0.48).
domain_priors:suppression_score(citizenship_clause__allegiance_qualified_reading, 0.62).
domain_priors:theater_ratio(citizenship_clause__allegiance_qualified_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citizenship_clause__allegiance_qualified_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(citizenship_clause__allegiance_qualified_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(citizenship_clause__allegiance_qualified_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citizenship_clause__allegiance_qualified_reading, tangled_rope).
narrative_ontology:human_readable(citizenship_clause__allegiance_qualified_reading, "Citizenship Clause — Allegiance-Qualified Reading").
narrative_ontology:topic_domain(citizenship_clause__allegiance_qualified_reading, "constitutional/doctrinal/citizenship").

domain_priors:requires_active_enforcement(citizenship_clause__allegiance_qualified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(citizenship_clause__allegiance_qualified_reading, '9dd1a667-fabb-4794-8d41-d915a512e4e1').
narrative_ontology:cs_kernel_codification('9dd1a667-fabb-4794-8d41-d915a512e4e1', fixed_text).
narrative_ontology:cs_authority_grounding('9dd1a667-fabb-4794-8d41-d915a512e4e1', lineage).
narrative_ontology:cs_interpretation_layer_present('9dd1a667-fabb-4794-8d41-d915a512e4e1').
narrative_ontology:cs_reading_relation('9dd1a667-fabb-4794-8d41-d915a512e4e1', citizenship_clause__birthright_territorial_reading, forecloses).
narrative_ontology:cs_reading_relation('9dd1a667-fabb-4794-8d41-d915a512e4e1', citizenship_clause__wong_kim_ark_settlement_reading, coexists_with).
narrative_ontology:cs_axiom('9dd1a667-fabb-4794-8d41-d915a512e4e1', foundational, allegiance_determinative_membership).
narrative_ontology:cs_axiom_status(allegiance_determinative_membership, holdable).
narrative_ontology:cs_axiom_grounding('9dd1a667-fabb-4794-8d41-d915a512e4e1', allegiance_determinative_membership, deontological).
narrative_ontology:cs_axiom('9dd1a667-fabb-4794-8d41-d915a512e4e1', foundational, jurisdiction_requires_political_tie).
narrative_ontology:cs_axiom_status(jurisdiction_requires_political_tie, holdable).
narrative_ontology:cs_axiom_grounding('9dd1a667-fabb-4794-8d41-d915a512e4e1', jurisdiction_requires_political_tie, deontological).
narrative_ontology:cs_reference_frame('9dd1a667-fabb-4794-8d41-d915a512e4e1', membership_by_consent_framework).
narrative_ontology:cs_drift_state('9dd1a667-fabb-4794-8d41-d915a512e4e1', contemporary_mass_immigration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9dd1a667-fabb-4794-8d41-d915a512e4e1', '').
narrative_ontology:cs_kernel_id(citizenship_clause__allegiance_qualified_reading, citizenship_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citizenship_clause__allegiance_qualified_reading, membership_by_consent_doctrine).
narrative_ontology:constraint_beneficiary(citizenship_clause__allegiance_qualified_reading, allegiance_based_polity).
narrative_ontology:constraint_victim(citizenship_clause__allegiance_qualified_reading, birthright_claims_temporarily_present).
narrative_ontology:constraint_victim(citizenship_clause__allegiance_qualified_reading, children_foreign_allegiance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHILD BORN TO FOREIGN ALLEGIANCE (SNARE) — Structurally mobile (could be recognized as citizen under territorial rule) but excluded by allegiance doctrine. No exit from the exclusion without parental allegiance shift or naturalization proceedings. Maximum extraction: citizenship conditional on a political tie the agent cannot unilaterally control. Bears the full cost of the allegiance requirement.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TEMPORARILY PRESENT RESIDENT (TANGLED ROPE) — Constrained by visa status and employment authorization, but also benefits from the allegiance doctrine if it protects skilled immigrant admission by conditioning citizenship on sustained political commitment. Extraction is real (citizenship delayed or denied) but coordinated with the state's interest in screening membership by allegiance rather than accident of location.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALLEGIANCE-BASED POLITY (ROPE) — Sees the constraint as pure coordination: citizenship is a membership contract, and requiring allegiance is the mechanism for screening who enters the body politic. The doctrine benefits the polity by conditioning membership on political tie rather than geographic accident. No experienced extraction — the constraint is the solution to the coordination problem of defining membership.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATELESS COHORT / GENERATIONAL VIEW (SNARE) — Over time, allegiance doctrine creates structural statelessness for cohorts born to foreign diplomats, transient workers, or occupied populations. Trapped across generations without path to citizenship. Maximum experienced extraction scaled by temporal horizon: each generation that passes locks the next generation into statelessness.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRATION RIGHTS ADVOCATES (TANGLED ROPE) — Organized agents advocating for expansive birthright. Constrained by constitutional text and precedent but also benefit from the doctrinal contest itself — the existence of a live interpretive question creates opportunity for advocacy and norm-shifting. See the constraint as a hybrid coordination (defining membership) and extraction (excluding based on parental status).
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURALIZATION PIPELINE (SCAFFOLD) — Institutional actors (immigration courts, USCIS) see allegiance doctrine as temporary: denial of birthright citizenship is addressed through structured naturalization, which offers a time-limited path to membership. The scaffold has a built-in sunset: after 5-10 years of residence and allegiance demonstration, citizenship is available. Low theater from this perspective — the naturalization process is functionally addressing the exclusion.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: THE DOCTRINE MACHINERY (PITON) — From a civilizational view, allegiance doctrine persists through legal momentum and textual authority rather than functional necessity. Modern citizenship is mostly territorial; allegiance is invoked as legitimating gloss but does minimal actual filtering work. Theater ratio high — the doctrine is maintained for its rhetorical authority over membership, not because it prevents actual problems. The machinery continues through institutional inertia.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some requirement of allegiance for membership is structurally inherent to political organization: a polity that accepts all territorially born persons regardless of political tie is an open-membership system that cannot sustain bounded citizenship. This perspective sees the allegiance requirement as an unchangeable feature of how political authority works. However, the structural data contradicts the mountain classification — the engine will flag this as a false summit, revealing that the 'inherent to political order' framing naturalizes a contestable doctrinal choice.
constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citizenship_clause__allegiance_qualified_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citizenship_clause__allegiance_qualified_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(citizenship_clause__allegiance_qualified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(citizenship_clause__allegiance_qualified_reading, TR),
    TR >= 0.70.

:- end_tests(citizenship_clause__allegiance_qualified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The allegiance requirement extracts from children born to foreign nationals by denying automatic citizenship despite birth on territory. The extraction is real — citizenship is withheld based on parental status the child cannot control at birth. However, it is not severe (ε ≥ 0.46 snare threshold but not high-snare at ≥0.66) because naturalization pathways exist, and the requirement has functional logic (screening for allegiance). The measurement trajectory (0.35 → 0.42 → 0.48) reflects rising extraction as immigration enforcement has intensified and stateless populations have grown larger. Suppression (0.62): Moderate-high. Suppression is structural: children born to foreign nationals face barriers to exit (cannot unilaterally shift parental allegiance), cannot claim birthright without parental status change, and have limited access to immediate naturalization. The trajectory (0.45 → 0.58 → 0.62) reflects increasing enforcement pressure and rising barriers to naturalization. Theater ratio (0.58): Moderate. The allegiance doctrine maintains significant performative content — the requirement is invoked as a legitimating principle for membership screening, but actual functional screening happens through immigration enforcement and naturalization procedures, not through allegiance doctrine itself. Modern practice is mostly territorial; allegiance is a rhetorical gloss covering what is functionally a residency requirement.
 *
 * PERSPECTIVAL GAP:
 *   The allegiance-qualified reading produces a wide perspectival gap because the same doctrine is experienced as pure coordination (by the polity), as extraction with limited escape (by moderate residents), as total exclusion (by powerless children), as functional injustice (by organized advocates), as a temporary administrative problem (by the naturalization pipeline), and as doctrinal theater (by civilizational analysis). The gap reveals the constraint's hybrid nature: it has genuine coordination content (the polity's legitimate interest in screening members by allegiance) layered with extraction (conditioning citizenship on status the agent cannot control). The tangled-rope classification is the stable point — higher than rope because extraction is real and suppression is high, lower than snare because coordination function is genuine and escape routes exist.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationships. Children born to foreign allegiance occupy the target position (victims of the exclusion rule) with trapped exit options — they cannot unilaterally change parental allegiance and face high barriers to alternative residence. This yields high d → high f(d) → high chi from their perspective, producing snare classification. The membership-by-consent doctrine occupies the beneficiary position (allegiance requirement enables its functioning) with arbitrage exit options (doctrinal positions can be revised, institutions can shift policy) — low d → low/negative f(d) → low chi from institutional perspective, producing rope. The analytical observer occupies a universal scope with analytical exit options, canonical d=0.72, producing f(d)≈1.15, chi≈0.55 at institutional scope — sufficient to classify as mountain if the constraint were genuinely unchangeable, but the structural data (identifiable beneficiaries, contestable doctrinal choice) falsifies the mountain classification, flagging this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy (false classification of coordination as extraction or vice versa) by explicitly declaring both the coordination function (membership screening) and the extraction mechanism (conditional citizenship). The tangled-rope classification honors both. The risk point is the false summit: if the allegiance requirement is framed as an unchangeable feature of political order (mountain), mandatrophy is triggered. The analytical perspective's mountain classification is explicitly marked as a false-summit candidate in the schema — beneficiaries are declared, triggering FSM evaluation by the engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegiance_vs_territorial_kernel,
    'Is allegiance or territory the determinative constitutional principle for birthright citizenship?',
    'Textual analysis of ''subject to the jurisdiction'' clause; historical intent at 14th Amendment ratification; doctrinal genealogy from English common law through Wong Kim Ark',
    'If allegiance is determinative: this reading''s classification holds; beneficiaries control membership screening. If territory is determinative: the territorial reading controls; the allegiance requirement becomes contingent overlay on underlying birthright rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegiance_vs_territorial_kernel, conceptual, 'Whether allegiance or territory is the foundational principle of the citizenship clause').

omega_variable(
    subject_to_jurisdiction_scope,
    'What does ''subject to the jurisdiction'' actually exclude? Diplomats and invading armies only, or a broader category including children of foreign nationals without permanent settlement?',
    'Case-law genealogy from Dred Scott through Wong Kim Ark to contemporary immigration law; functional analysis of who exercises jurisdiction over which persons; statutory interpretation of ''jurisdiction'' in other constitutional contexts',
    'Narrow reading (diplomats/armies only): territorial reading wins, allegiance doctrine becomes subordinate gloss. Broad reading (includes foreign-allegiance category): this reading''s structure holds, allegiance becomes determinative filter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subject_to_jurisdiction_scope, conceptual, 'Scope of exclusion from ''subject to the jurisdiction''').

omega_variable(
    wong_kim_ark_settlement_lock,
    'Does Wong Kim Ark''s 1898 settlement foreclose reinterpreting the clause through the allegiance lens, or does it merely establish a strong presumption that can be revisited?',
    'Doctrine-of-precedent analysis; examination of whether Wong Kim Ark opinion itself treated territorial rule as unquestionable or as a reasoned interpretation; assessment of whether subsequent doctrine has treated the precedent as binding on the allegiance question or merely as settling the specific issue (children of Chinese laborers)',
    'If Wong Kim Ark forecloses: allegiance reading is doctrinally impossible; this reading is foreclosed by a sibling (wong_kim_ark_settlement_reading). If precedent is revisable: this reading coexists with the settlement reading as a live alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wong_kim_ark_settlement_lock, conceptual, 'Whether Wong Kim Ark forecloses or merely influences the allegiance reading').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the allegiance requirement a coordination mechanism (polity screening for members willing to pledge allegiance) or extraction (conditioning citizenship on status the child cannot control)?',
    'Empirical: do children born to foreign nationals actually have exit options (parents can shift allegiance, children can naturalize after age of majority, temporary residents become permanent)? Normative: is a requirement imposed on persons without consent to the contract a legitimate coordination condition?',
    'If coordination dominates: constraint lowers toward Rope (beneficiary''s perspective). If extraction dominates: constraint rises toward Snare (victim''s perspective). Current ε=0.48 reflects genuine hybridity; omega resolution could shift terminal classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether allegiance requirement is coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citizenship_clause__allegiance_qualified_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(citz_alleg_tr_t0, citizenship_clause__allegiance_qualified_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(citz_alleg_tr_t20, citizenship_clause__allegiance_qualified_reading, theater_ratio, 20, 0.54).
narrative_ontology:measurement(citz_alleg_tr_t50, citizenship_clause__allegiance_qualified_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(citz_alleg_be_t0, citizenship_clause__allegiance_qualified_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(citz_alleg_be_t20, citizenship_clause__allegiance_qualified_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(citz_alleg_be_t50, citizenship_clause__allegiance_qualified_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(citz_alleg_su_t0, citizenship_clause__allegiance_qualified_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(citz_alleg_su_t20, citizenship_clause__allegiance_qualified_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(citz_alleg_su_t50, citizenship_clause__allegiance_qualified_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citizenship_clause__allegiance_qualified_reading, identity_coordination).
narrative_ontology:affects_constraint(citizenship_clause__allegiance_qualified_reading, citizenship_clause__birthright_territorial_reading).
narrative_ontology:affects_constraint(citizenship_clause__allegiance_qualified_reading, citizenship_clause__wong_kim_ark_settlement_reading).
narrative_ontology:affects_constraint(citizenship_clause__allegiance_qualified_reading, naturalization_allegiance_requirement).
narrative_ontology:affects_constraint(citizenship_clause__allegiance_qualified_reading, diplomatic_immunity_jurisdiction).

% DUAL FORMULATION NOTE:
% This story instantiates the allegiance-qualified reading of the citizenship clause. The birthright_territorial_reading and wong_kim_ark_settlement_reading are separate constraint stories representing sibling interpretations of the same kernel. All three share the same base constitutional text but decompose it into distinct claims with different ε values and different beneficiary/victim structures. The allegiance reading has ε=0.48 (tangled rope); the territorial reading has lower ε and different suppression (pure coordination); the settlement reading locks both into precedential form with high theater (doctrinal inertia). Network links show the kernel contest structure: all three stories affect each other through doctrinal competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
