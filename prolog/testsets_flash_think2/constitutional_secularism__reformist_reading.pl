% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Reading of Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'reformist reading' of constitutional
 *   secularism, which posits an affirmative duty for the state to actively
 *   intervene and eliminate religious practices that oppress marginalized
 *   groups, even if it supersedes claims of religious autonomy. This reading
 *   is highly contested, particularly by religious conservatives, but is
 *   gaining traction among social justice advocates and some constitutional
 *   scholars. It is one reading of the broader 'constitutional_secularism'
 *   kernel, distinct from 'strict_neutrality_reading' and
 *   'principled_intervention_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.85).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.9).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Reading of Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '51cddf15-a5d1-46a3-b2e9-d59cb5d7b764').
narrative_ontology:cs_kernel_codification('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', fixed_text).
narrative_ontology:cs_authority_grounding('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', lineage).
narrative_ontology:cs_interpretation_layer_present('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764').
narrative_ontology:cs_reading_relation('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', foundational, social_equality_as_constitutional_imperative).
narrative_ontology:cs_axiom_status(social_equality_as_constitutional_imperative, holdable).
narrative_ontology:cs_axiom_grounding('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', social_equality_as_constitutional_imperative, deontological).
narrative_ontology:cs_axiom('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', foundational, state_as_primary_agent_of_social_reform).
narrative_ontology:cs_axiom_status(state_as_primary_agent_of_social_reform, holdable).
narrative_ontology:cs_axiom_grounding('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', state_as_primary_agent_of_social_reform, instrumental).
narrative_ontology:cs_reference_frame('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', constitutional_social_justice_mandate).
narrative_ontology:cs_drift_state('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', contemporary_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('51cddf15-a5d1-46a3-b2e9-d59cb5d7b764', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_within_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, marginalized_religious_groups).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, traditionalist_religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The judiciary and legislature, interpreting the constitution to mandate active state intervention to dismantle religious practices deemed oppressive. They initiate and enforce reforms, facing political and social resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_constitutional_actors, agenda_setter,
    institutional, generational, analytical, national).

% Historically and currently subjected to discriminatory religious practices. This reading offers them state protection and a pathway to social equality, but they remain vulnerable without active enforcement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    powerless, generational, trapped, national).

% Often subject to patriarchal religious norms and practices that limit their autonomy and rights. This reading provides a legal basis for challenging and eliminating such practices, offering a path to greater equality.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_within_religious_communities, beneficiary,
    powerless, generational, trapped, national).

% Religious minorities or sects whose practices may be protected by this reading if they are themselves targets of oppression, or who benefit from a more equitable social landscape. They may also face scrutiny if their own practices are deemed oppressive.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, marginalized_religious_groups, beneficiary,
    moderate, biographical, constrained, national).

% Adherents to traditional religious practices that are targeted for elimination under this reading. They experience a loss of religious autonomy and cultural continuity, often mobilizing significant resistance against state intervention.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, generational, constrained, national).

% Organizations and bodies that uphold and enforce traditional religious practices. They face direct state intervention, legal challenges, and a loss of authority over their internal affairs, leading to significant institutional resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, traditionalist_religious_institutions, payer,
    institutional, generational, constrained, national).

% Groups and scholars who argue for the state maintaining equal distance from all religions, without preferential treatment or interference. This reading's active intervention directly contradicts their core principle, effectively excluding their approach from the policy discourse.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, strict_neutrality_advocates, excluded,
    organized, biographical, mobile, national).

% Groups and scholars who support state intervention in religious affairs for social reform, but perhaps with more caution or a higher threshold than the reformist reading. They observe the implementation of this reading, potentially influencing its scope or methods.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, principled_intervention_advocates, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate social justice and equality across diverse religious communities by establishing a constitutional baseline of non-oppression, ensuring that religious autonomy does not shield practices harmful to marginalized groups.
% TRANSFER_FUNCTION: Transfers authority over religious practices from traditional religious institutions and conservative adherents to the state and marginalized groups, in exchange for enhanced social equality and human rights.
% ABSENT_VOICES: Religious groups advocating for absolute autonomy from state interference, or those who believe that state intervention in religious matters is inherently illegitimate, regardless of its stated goals. Their arguments are often dismissed as upholding oppression.
% DISAPPEARANCE_RATIONALE: If this affirmative duty vanished, oppressive religious practices would likely reassert themselves without state checks, and marginalized groups would lose a crucial legal and constitutional avenue for protection and reform, leading to a significant rearrangement of social power dynamics and rights.
% FOUNDING_PROBLEM: The historical and ongoing oppression of marginalized groups (e.g., scheduled castes, women) within religious communities, where claims of religious autonomy were used to shield discriminatory and harmful practices from state regulation or reform.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, social justice movements, academic studies on caste discrimination and gender inequality in religious contexts, and direct testimony from affected marginalized communities consistently corroborate the persistence of these founding problems.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading mandates direct state action to dismantle deeply embedded religious practices, fundamentally altering the autonomy of religious communities. Suppression (0.90) is also very high, as the state must actively enforce these reforms against significant resistance, often through legal and coercive means. The theater ratio (0.10) is low, reflecting that this is an active, functional duty, not a performative one; the state genuinely seeks to eliminate these practices. Resistance (0.95) is extremely high, as it challenges core identity and tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups and reformist advocates, this constraint is a necessary and just mechanism for achieving social equality and human rights. From the perspective of religious conservatives and traditionalist institutions, it is an oppressive overreach of state power, an attack on religious freedom, and a violation of community autonomy. The engine's classification will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The state constitutional actors are the agenda-setters, benefiting from the expansion of state power and the fulfillment of a perceived constitutional mandate. Marginalized groups (scheduled castes, women, other marginalized religious groups) are the primary beneficiaries, gaining protection and equality. Religious conservatives and traditionalist religious institutions are the clear targets/victims, experiencing significant extraction of their autonomy and authority. Advocates for strict neutrality are excluded, as their position is fundamentally incompatible with this active state duty.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (eliminating oppression) is considered live by its proponents, preventing a mandatrophy classification. However, opponents argue that the state's methods or scope of intervention have outlived their justification, turning a legitimate concern into an extractive power grab. The high resistance and contested founding problem status reflect this ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_oppressive_practices,
    'How is ''oppressive'' defined in practice, and what criteria are used to distinguish genuinely harmful religious practices from merely unpopular or traditional ones?',
    'Judicial precedent and legislative guidelines that provide clear, consistent, and narrowly tailored definitions, subject to public and expert review.',
    'A broad or vague definition could lead to overreach and increased extraction from religious communities, potentially shifting the classification towards a Snare. A narrow, well-defined scope would reinforce the Tangled Rope classification by focusing on genuine harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_oppressive_practices, conceptual, 'Ambiguity in defining ''oppressive'' religious practices.').

omega_variable(
    effectiveness_of_state_intervention,
    'Does state intervention, as mandated by this reading, effectively eliminate oppressive practices and improve the conditions of marginalized groups, or does it lead to unintended consequences or backlash?',
    'Longitudinal sociological and anthropological studies tracking the impact of specific state interventions on targeted practices and affected communities, including measures of social change and resistance.',
    'If interventions are largely ineffective or counterproductive, the coordination function (social reform) would be undermined, increasing the effective extraction and potentially reclassifying towards a Snare. If highly effective, it would strengthen the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_state_intervention, empirical, 'Empirical effectiveness of state-mandated reforms.').

omega_variable(
    legitimacy_of_state_supersession,
    'Is the state''s supersession of religious autonomy claims, even for social justice, a legitimate exercise of constitutional power, or does it fundamentally violate a core principle of religious freedom?',
    'Ongoing philosophical and legal debate, potentially influenced by international human rights jurisprudence and evolving societal values regarding the balance between collective rights and individual/group autonomy.',
    'If the supersession is widely deemed illegitimate, the constraint''s perceived coordination function would collapse for many, making it appear as pure extraction (Snare). If widely accepted, it reinforces the legitimacy of the state''s role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_state_supersession, preference, 'Normative legitimacy of state overriding religious autonomy.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint truly a distinct reading of constitutional secularism, or is it merely a more aggressive application of the ''principled_intervention_reading''?',
    'Analysis of judicial opinions and legislative debates to identify explicit foundational differences in legal reasoning and stated intent between the ''affirmative duty to eliminate'' and ''may intervene to advance reform'' positions.',
    'If not distinct, it suggests a continuum rather than discrete readings, potentially collapsing the ''reformist_reading'' into a more extreme variant of ''principled_intervention_reading''. If distinct, it highlights a fundamental divergence in constitutional interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinction between reformist and principled intervention readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_secularism__reformist_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(cons_tr_t1970, constitutional_secularism__reformist_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(cons_tr_t1990, constitutional_secularism__reformist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(cons_tr_t2010, constitutional_secularism__reformist_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(cons_tr_t2025, constitutional_secularism__reformist_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__reformist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__reformist_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(cons_be_t1990, constitutional_secularism__reformist_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__reformist_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(cons_be_t2025, constitutional_secularism__reformist_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__reformist_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__reformist_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(cons_su_t1990, constitutional_secularism__reformist_reading, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__reformist_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(cons_su_t2025, constitutional_secularism__reformist_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
