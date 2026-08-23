% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty Border Control Norm
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   The qualified sovereignty reading holds that states retain border control
 *   authority but must exercise it proportionately to legitimate state
 *   interests and consistently with human rights obligations. This is one of
 *   three readings of the contested kernel 'border_normative_status'. The
 *   sovereignty_primary reading treats territorial exclusion as a
 *   foundational attribute of collective self-determination with minimal
 *   external constraint. The freedom_primary reading treats freedom of
 *   movement as a fundamental right that borders impermissibly restrict. The
 *   qualified_sovereignty reading occupies the middle ground instantiated in
 *   international human rights law: state authority is real but conditioned.
 *   This constraint story captures the qualified_sovereignty reading as a
 *   clean, ε-invariant constraint with its own beneficiary/victim structure
 *   and adjudication burden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.58).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.62).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty Border Control Norm").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '5946c6b4-d127-4230-a557-639f6dece8ec').
narrative_ontology:cs_kernel_codification('5946c6b4-d127-4230-a557-639f6dece8ec', formalized).
narrative_ontology:cs_authority_grounding('5946c6b4-d127-4230-a557-639f6dece8ec', lineage).
narrative_ontology:cs_interpretation_layer_present('5946c6b4-d127-4230-a557-639f6dece8ec').
narrative_ontology:cs_reading_relation('5946c6b4-d127-4230-a557-639f6dece8ec', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('5946c6b4-d127-4230-a557-639f6dece8ec', border_normative_status__freedom_primary, influences).
narrative_ontology:cs_axiom('5946c6b4-d127-4230-a557-639f6dece8ec', foundational, proportionality_required_for_exclusion).
narrative_ontology:cs_axiom_status(proportionality_required_for_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('5946c6b4-d127-4230-a557-639f6dece8ec', proportionality_required_for_exclusion, conventional).
narrative_ontology:cs_axiom('5946c6b4-d127-4230-a557-639f6dece8ec', foundational, human_rights_limit_sovereignty).
narrative_ontology:cs_axiom_status(human_rights_limit_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5946c6b4-d127-4230-a557-639f6dece8ec', human_rights_limit_sovereignty, deontological).
narrative_ontology:cs_reference_frame('5946c6b4-d127-4230-a557-639f6dece8ec', post_udhr_proportionality_framework).
narrative_ontology:cs_drift_state('5946c6b4-d127-4230-a557-639f6dece8ec', contemporary_securitization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5946c6b4-d127-4230-a557-639f6dece8ec', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, states).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, displaced_citizens).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, proportionality_principle).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, human_rights_obligations).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, legitimate_state_interests_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain authority to control borders but must justify each exercise through proportionality review: the measure must pursue a legitimate aim, be necessary, and not impose disproportionate burdens on rights-holders. States administer the adjudication machinery (courts, tribunals, administrative review) and bear the institutional cost of compliance. They can exit by withdrawing from treaty regimes but face reputational and diplomatic costs.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, states, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the direct costs of border enforcement: denial of entry, detention, removal, family separation, and loss of livelihood. Their ability to challenge exclusion depends on access to legal representation, which is structurally limited by detention, language barriers, and jurisdictional hurdles. Exit from the constraint means either successful entry (winning the proportionality challenge) or remaining in origin/transit countries — both are blocked by the very constraint they contest.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Citizens displaced by border policies (e.g., family members separated by admission denials, communities affected by enforcement operations, citizens wrongly detained/deported). They benefit from state security and border integrity but pay when proportionality review fails or is performative. Their exit options are political (voting, advocacy) and legal (domestic courts) but constrained by national identity and lack of mobility alternatives.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, displaced_citizens, beneficiary).

% UN treaty bodies, special rapporteurs, regional human rights courts monitor state compliance with proportionality obligations. They issue authoritative interpretations, conduct country reviews, and hear individual communications. They do not enforce directly but shape the legitimacy conditions under which states operate. Their authority derives from the treaty system states have joined.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Domestic and regional courts (ECtHR, IACtHR, ACHPR, national supreme courts) adjudicate proportionality challenges. They set the doctrinal standards for what counts as legitimate aim, necessity, and proportionality stricto sensu. Their rulings create binding precedent that shapes future state action. They are neither pure beneficiaries nor payers but the institutional engine of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, courts_tribunals, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, courts_tribunals, observer).

% Lack nationality and therefore fall outside the citizen/migrant binary that structures the proportionality framework. They are excluded from the adjudication process because they have no state to claim them and no standing in most treaty bodies. Their situation would deteriorate if the constraint disappeared (no state would admit them) but also persists because the constraint's categories do not recognize them.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, stateless_persons, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating the legitimate exercise of border control with the protection of human rights: states need to control entry for security, public order, and resource management, while migrants and citizens need protection from arbitrary exclusion. Proportionality review provides the structured adjudication mechanism that makes this coordination possible without collapsing into either open borders or unbounded exclusion.
% TRANSFER_FUNCTION: Moves the power to exclude from an unbounded state prerogative to a constrained, reviewable authority. The transfer runs from migrants and displaced citizens (who lose the protection of a presumption against exclusion) to states (who gain a structured framework that legitimates their remaining exclusionary power). Human rights bodies and courts receive the adjudicative authority to police the boundary.
% ABSENT_VOICES: Stateless persons, who fall outside the citizen/migrant binary and have no standing in the proportionality framework; future generations who will inherit the border regimes shaped by today's proportionality jurisprudence; irregular migrants in transit who are intercepted before reaching any adjudicative forum.
% DISAPPEARANCE_RATIONALE: If the proportionality constraint vanished overnight, states would revert to unbounded exclusionary authority — mass expulsions, categorical bars, and suspension of asylum would become legally unchallengeable at the international level. If state border authority vanished, the international migration regime would collapse into open movement with no coordination mechanism for reception, integration, or security screening. The world rearranges in either direction.
% FOUNDING_PROBLEM: How to reconcile the Westphalian principle of state sovereignty over territory with the post-WWII human rights revolution that made individuals subjects of international law. The 1951 Refugee Convention, ICCPR, and regional human rights treaties created a tension: states claim the right to exclude non-nationals, but human rights law limits that right when exclusion would violate non-refoulement, family unity, or proportionality.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and the UN Human Rights Committee attest the founding problem persists: states routinely invoke 'border security' to justify measures that treaty bodies find disproportionate. The International Law Commission's work on expulsion of aliens documents the ongoing contestation. Conversely, the European Court of Human Rights' Grand Chamber in N.D. and N.T. v. Spain (2020) and the Inter-American Court's advisory opinion OC-21/14 show the proportionality framework remains the operative coordination mechanism, not a dead letter.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the real transfer: states retain substantial exclusionary power while migrants and displaced citizens bear the costs of enforcement. The extractiveness has risen steadily from 1948 (0.25) as border enforcement infrastructure expanded (detention, interdiction, externalization) while proportionality review has not kept pace. Suppression (0.62) is high because the constraint's persistence depends on states maintaining the adjudication machinery (courts, tribunals, reporting) while simultaneously building enforcement capacity that outruns review. Theater ratio (0.42) is significant: proportionality review exists and produces real jurisprudence, but a growing share of border enforcement (externalization, pushbacks, detention without review) operates outside the adjudicative framework the constraint claims to require. Accessibility collapse (0.55) is moderate: alternatives (open borders, regional free movement, categorical asylum) exist conceptually but are politically foreclosed. Resistance (0.58) is substantial: migrants resist through litigation, advocacy, and physical mobility; states resist through non-compliance, treaty withdrawal threats, and forum shopping.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharp seat divergence: from the state seat, the constraint appears as genuine coordination (rope-like) — a framework that legitimates necessary exclusion. From the excluded migrant seat, it appears as enforced extraction (snare-like) — a review mechanism that legitimates most exclusions while failing to prevent the worst. From the displaced citizen seat, it appears as a tangled rope — real protection that sometimes works but often fails. The analytical observer seat sees the full structure: a constraint that coordinates by creating an adjudication burden, but whose enforcement has steadily decoupled from its coordination rationale.
 *
 * DIRECTIONALITY LOGIC:
 *   States are the structural beneficiaries (d ~0.15): they collect the exclusionary authority and control the adjudication machinery, though they bear compliance costs. Excluded migrants are full targets (d ~0.95): they bear the extraction, have trapped exit, and are powerless. Displaced citizens are partial targets (d ~0.7): they bear costs when proportionality fails but retain political exit options. Human rights bodies and courts are analytical observers (d ~0.5): they neither collect nor pay but constitute the constraint's enforcement architecture. Stateless persons are excluded from the directionality derivation entirely — their structural position is not captured by the beneficiary/victim binary, which is itself an omega variable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling sovereignty with human rights) remains live and contested — states claim the problem is new (mass migration, terrorism, smuggling) while human rights bodies claim the problem is old (the same proportionality standard applies). The constraint has not resolved into pure coordination (rope) because extraction has accumulated; it has not collapsed into pure extraction (snare) because proportionality review still blocks some measures and creates doctrinal friction. The adjudication burden on states is the constraint's distinguishing feature: it is not self-executing but requires continuous institutional maintenance. This makes it a tangled rope — the coordination function (proportionality review) and extraction function (border enforcement) are structurally fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the qualified_sovereignty reading end and the sovereignty_primary reading begin? At what threshold of deference to state security claims does proportionality review collapse into sovereignty_primary?',
    'Track the margin of appreciation doctrine in regional courts: as the margin widens, the qualified_sovereignty reading converges toward sovereignty_primary. A systematic study of ECtHR, IACtHR, and ACHPR case law coding deference levels would quantify the boundary.',
    'If the boundary is porous, the three readings may not be structurally distinct constraints but a single constraint with a continuous parameter — violating ε-invariance. If the boundary is sharp (e.g., a specific doctrinal threshold), the decomposition is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the three kernel readings are structurally distinct constraints or a single constraint with a continuous deference parameter.').

omega_variable(
    proportionality_standard_ambiguity,
    'Is proportionality a single coherent standard (legitimate aim, necessity, proportionality stricto sensu) or a variable framework whose stringency shifts with the political context?',
    'Comparative doctrinal analysis across jurisdictions and time: code proportionality assessments for structural similarity. If the test''s structure is stable but outcomes vary, it is a single standard with variable application. If the test''s structure itself mutates (e.g., necessity drops out, balancing replaces structured review), it is a variable framework.',
    'If proportionality is a variable framework, the constraint''s extractiveness is not stable — it depends on which version of proportionality is operative. This would require decomposing qualified_sovereignty into sub-readings (e.g., structured_proportionality vs. deferential_balancing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_standard_ambiguity, empirical, 'Whether the proportionality standard at the heart of this reading is structurally unitary or contextually variable.').

omega_variable(
    stateless_exclusion_from_framework,
    'Does the qualified_sovereignty reading''s binary (citizen/migrant) structurally exclude stateless persons, or can the framework be extended to cover them without changing its character?',
    'Analyze whether existing proportionality jurisprudence has been extended to stateless persons (e.g., through ''effective nationality'' doctrines, habitual residence tests, or human rights treaty body interpretations). If extensions exist and function, the exclusion is contingent. If the framework''s logic requires the binary, the exclusion is structural.',
    'If structural, the qualified_sovereignty reading has an internal exclusion that mirrors the sovereignty_primary reading''s exclusion — the constraint extracts from a population it does not recognize as rights-holders. This would support a snare-like classification for the stateless sub-population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stateless_exclusion_from_framework, conceptual, 'Whether stateless persons are contingently or structurally excluded from the proportionality framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by excluded migrants structural (detention, legal bars, interdiction) or internalized (migrants self-censor, avoid claiming rights, accept exclusion as inevitable)?',
    'Post-exit suppression trajectory: study migrants who successfully enter and regularize — do they continue to self-censor or do they exercise rights? Compare suppression levels for similarly situated migrants with and without access to legal representation.',
    'If internalized suppression is significant, the measured suppression (0.62) understates the constraint''s effective grip — the target carries the suppression with them after formal exit. This would amplify effective extraction for the migrant seat beyond what structural measures capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_qualified_sov_tr_t1948, border_normative_status__qualified_sovereignty, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(border_qualified_sov_tr_t1966, border_normative_status__qualified_sovereignty, theater_ratio, 1966, 0.18).
narrative_ontology:measurement(border_qualified_sov_tr_t1984, border_normative_status__qualified_sovereignty, theater_ratio, 1984, 0.25).
narrative_ontology:measurement(border_qualified_sov_tr_t1990, border_normative_status__qualified_sovereignty, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(border_qualified_sov_tr_t2001, border_normative_status__qualified_sovereignty, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(border_qualified_sov_tr_t2015, border_normative_status__qualified_sovereignty, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(border_qualified_sov_tr_t2024, border_normative_status__qualified_sovereignty, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(border_qualified_sov_be_t1948, border_normative_status__qualified_sovereignty, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement(border_qualified_sov_be_t1966, border_normative_status__qualified_sovereignty, base_extractiveness, 1966, 0.3).
narrative_ontology:measurement(border_qualified_sov_be_t1984, border_normative_status__qualified_sovereignty, base_extractiveness, 1984, 0.38).
narrative_ontology:measurement(border_qualified_sov_be_t1990, border_normative_status__qualified_sovereignty, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(border_qualified_sov_be_t2001, border_normative_status__qualified_sovereignty, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(border_qualified_sov_be_t2015, border_normative_status__qualified_sovereignty, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(border_qualified_sov_be_t2024, border_normative_status__qualified_sovereignty, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(border_qualified_sov_su_t1948, border_normative_status__qualified_sovereignty, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(border_qualified_sov_su_t1966, border_normative_status__qualified_sovereignty, suppression_requirement, 1966, 0.35).
narrative_ontology:measurement(border_qualified_sov_su_t1984, border_normative_status__qualified_sovereignty, suppression_requirement, 1984, 0.45).
narrative_ontology:measurement(border_qualified_sov_su_t1990, border_normative_status__qualified_sovereignty, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(border_qualified_sov_su_t2001, border_normative_status__qualified_sovereignty, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(border_qualified_sov_su_t2015, border_normative_status__qualified_sovereignty, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement(border_qualified_sov_su_t2024, border_normative_status__qualified_sovereignty, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__qualified_sovereignty, 0.1).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, asylum_non_refoulement).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, detention_standards).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, family_unity_protection).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_externalization_agreements).

% DUAL FORMULATION NOTE:
% Part of the border_normative_status constraint family with sibling readings sovereignty_primary and freedom_primary. This reading (qualified_sovereignty) instantiates the proportionality framework of international human rights law. The sovereignty_primary reading instantiates the Westphalian exclusionary authority. The freedom_primary reading instantiates the freedom of movement as fundamental right. The three readings share the kernel but have different ε values, different beneficiary/victim structures, and different classifications. qualified_sovereignty is tangled_rope (coordination + extraction + enforcement); sovereignty_primary tends toward mountain (claimed) or piton (degraded); freedom_primary tends toward scaffold (transitional) or snare (when enforced as open borders without coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, institutional, 0.15).
constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, powerless, 0.95).
constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
