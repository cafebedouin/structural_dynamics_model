% ============================================================================
% CONSTRAINT STORY: twenty_second_amendment__democratic_choice_objection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_twenty_second_amendment__democratic_choice_objection_reading, []).

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
 *   constraint_id: twenty_second_amendment__democratic_choice_objection_reading
 *   human_readable: Twenty-Second Amendment: Democratic Choice Objection Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The Twenty-Second Amendment (ratified in 1951) prohibits any person from
 *   being elected to the presidency more than twice. This constraint embodies
 *   a fundamental tension in democratic constitutionalism: the text
 *   permanently forecloses an electoral option—whoever the people might want
 *   a third time is forbidden in advance. From the democratic choice
 *   objection reading, the amendment functions as distrust of the demos
 *   written into the foundational document. The voter cannot change this rule
 *   through electoral means alone; a supermajority (2/3 Congress, 3/4 states)
 *   must affirmatively repeal it. The suppression is asymmetric: all other
 *   elected offices remain subject to term-limit contestation (governors,
 *   senators, representatives), but the presidency is locked. The constraint
 *   extracts value by permanently removing a category of choice from every
 *   future ballot. The beneficiary is precommitment constitutionalism—the
 *   institutional framework that benefits from binding successive generations
 *   to rotation norms. The victim is electoral sovereignty at its widest: the
 *   aggregate power of the demos to choose their executive for any duration
 *   they prefer. The mechanism is not actively enforced (no Supreme Court has
 *   ruled on a third-term candidacy); instead, it persists through norm
 *   internalization and constitutional reverence. This explains the elevated
 *   theater ratio: the constraint maintains itself through ritualized
 *   compliance rather than coercive machinery.
 *
 * KEY AGENTS:
 *   - The Voter (Powerless/Trapped): Permanently barred from voting for a third-term candidate; no exit or workaround; bears full suppression cost
 *   - Precommitment Constitutionalism (Institutional/Arbitrage): Benefits from binding rotation norms; coordinating principle that depends on constitutional stability
 *   - The Electorate Aggregate (Powerless/Trapped): Electoral sovereignty foreclosed; the collective demos cannot override the constraint through ordinary democratic means
 *   - Congress and Political Parties (Organized/Constrained): Both benefit (rotation creates advancement opportunities) and bear costs (constrained ability to retain executive leadership); face supermajority barrier to repeal
 *   - The Presidency Across Time (Institutional/Constrained): Second-term president loses amendment capacity to continue; leverage declines after re-election
 *   - Constitutional Authority (Institutional/Arbitrage): Enforces through text and norm; benefits from text stability and inertial compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(twenty_second_amendment__democratic_choice_objection_reading, 0.62).
domain_priors:suppression_score(twenty_second_amendment__democratic_choice_objection_reading, 0.68).
domain_priors:theater_ratio(twenty_second_amendment__democratic_choice_objection_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(twenty_second_amendment__democratic_choice_objection_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(twenty_second_amendment__democratic_choice_objection_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(twenty_second_amendment__democratic_choice_objection_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(twenty_second_amendment__democratic_choice_objection_reading, snare).
narrative_ontology:human_readable(twenty_second_amendment__democratic_choice_objection_reading, "Twenty-Second Amendment: Democratic Choice Objection Reading").
narrative_ontology:topic_domain(twenty_second_amendment__democratic_choice_objection_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(twenty_second_amendment__democratic_choice_objection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(twenty_second_amendment__democratic_choice_objection_reading, '0606c83a-c61a-4ccb-bae7-f91a4e5fd617').
narrative_ontology:cs_kernel_codification('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', formalized).
narrative_ontology:cs_authority_grounding('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', lineage).
narrative_ontology:cs_interpretation_layer_present('0606c83a-c61a-4ccb-bae7-f91a4e5fd617').
narrative_ontology:cs_reading_relation('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', twenty_second_amendment__anti_caesarism_reading, coexists_with).
narrative_ontology:cs_reading_relation('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', twenty_second_amendment__lame_duck_cost_reading, influences).
narrative_ontology:cs_axiom('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', foundational, voter_choice_suppression_is_extraction).
narrative_ontology:cs_axiom_status(voter_choice_suppression_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', voter_choice_suppression_is_extraction, deontological).
narrative_ontology:cs_axiom('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', secondary, constitutional_precommitment_requires_distrust).
narrative_ontology:cs_axiom_status(constitutional_precommitment_requires_distrust, holdable).
narrative_ontology:cs_axiom_grounding('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', constitutional_precommitment_requires_distrust, deontological).
narrative_ontology:cs_reference_frame('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', voter_choice_primacy_framework).
narrative_ontology:cs_drift_state('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', contemporary_constitutional_interpretation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0606c83a-c61a-4ccb-bae7-f91a4e5fd617', '').
narrative_ontology:cs_kernel_id(twenty_second_amendment__democratic_choice_objection_reading, twenty_second_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(twenty_second_amendment__democratic_choice_objection_reading, precommitment_constitutionalism).
narrative_ontology:constraint_beneficiary(twenty_second_amendment__democratic_choice_objection_reading, institutional_rotation_norm).
narrative_ontology:constraint_victim(twenty_second_amendment__democratic_choice_objection_reading, voter_choice_category).
narrative_ontology:constraint_victim(twenty_second_amendment__democratic_choice_objection_reading, electoral_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOTER PERMANENTLY BARRED (SNARE) — Cannot exit the constraint or change its terms through democratic means alone. The suppression is constitutional: a two-thirds supermajority and state ratification are required to overturn the amendment. A voter who prefers a particular candidate for a third term faces absolute foreclosure — no exit option, no workaround, no electoral path. Powerless agents experience maximum extracted loss: the option itself is removed from every future ballot.
constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECOMMITMENT CONSTITUTIONALISM (ROPE) — Benefits from the constraint through institutional design: the amendment coordinates on rotation norms and prevents future democratic deviation. This perspective sees the constraint as pure coordination — a binding agreement to constrain majority choice in service of a higher constitutional commitment. The beneficiary has arbitrage options: can amend the constitution if the frame changes, can interpret clause scope narrowly. Experiences the constraint as beneficial institutional structure, not extraction.
constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS AND PARTIES AS SECONDARY ACTORS (TANGLED ROPE) — Both benefit (rotation creates regular advancement opportunities) and bear costs (constrained ability to retain strong executive leadership). A president in their second term cannot run again, reducing party incentive to preserve that presidency's influence. Congress has constrained exit: could move to repeal, but faces supermajority requirement and must build amendment coalition. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, the constraint appears immutable: democracies necessarily limit majority choice to preserve institutional stability. The amendment codifies what is presented as an inherent structural limit to democratic action — not extraction but natural law. However, the structural data reveals this as a false summit: the constraint has identifiable beneficiaries (precommitment constitutionalism) and victims (voter choice), contradicting the natural law classification.
constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: ENFORCEMENT AND RITUAL COMPLIANCE (PITON) — The enforcement of term limits is substantially performative at the electoral level: the norm is internalized (no president attempts to run for a third term) rather than actively enforced by courts. The Supreme Court has never needed to rule on a third-term candidacy. The constraint persists through ritualized acceptance and inertial institutional respect rather than active enforcement machinery. Theater ratio reflects that the mechanism maintains itself through constitutional reverence and norm internalization rather than coercive action.
constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(twenty_second_amendment__democratic_choice_objection_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(twenty_second_amendment__democratic_choice_objection_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(twenty_second_amendment__democratic_choice_objection_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(twenty_second_amendment__democratic_choice_objection_reading, TR),
    TR >= 0.70.

:- end_tests(twenty_second_amendment__democratic_choice_objection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The constraint removes a future electoral option permanently and irreversibly through ordinary democratic means. Every candidate who might have won a third-term election is suppressed in advance. The extraction value increases over time as the norm becomes internalized—initial enforcement costs decline, leaving pure suppression. Suppression (0.68): High. The suppression mechanism has two layers: (1) constitutional text (requires supermajority to overturn), and (2) internalized norm (no candidate even attempts to breach). The voter cannot exit through voting, electoral mobilization, or ordinary politics. Only constitutional amendment (extraordinarily difficult) provides escape. Theater ratio (0.35): Moderate. The constraint is actively maintained through constitutional jurisprudence and norm internalization rather than performed compliance. No Supreme Court action is needed; the norm is so internalized that the mechanism is transparent rather than theatrical. If the norm ever cracked (a candidate attempted to run for a third term), the theater would rise as courts and constitutional authorities actively enforced the text. Current stability reflects deep normalization.
 *
 * PERSPECTIVAL GAP:
 *   The democratic choice objection reading generates maximum perspectival disagreement. The voter sees snare—permanent foreclosure with no exit. Precommitment constitutionalism sees rope—beneficial coordination for institutional stability. Congress sees tangled rope—mixed benefits and costs. The analytical observer at civilizational scope risks seeing mountain (natural law of democratic bounds), but structural data (beneficiary + victim + specific text) reveals this as false summit. The enforcement apparatus sees piton—ritualized norm maintenance without active coercion. The reading contest (see omega: reading_contest_identity) shows that the anti_caesarism_reading and lame_duck_cost_reading would produce tangled_rope classifications from some perspectives (emphasizing coordination benefits), while this reading emphasizes pure suppression (snare from voter view, rope from institutional view).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading isolates the suppression mechanism and its cost to electoral sovereignty. The voter experiences maximum extraction (d=1.0, trapped exit, powerless power) because they bear the full cost of the removed option with no compensating benefit and no exit mechanism. Precommitment constitutionalism experiences negative extraction (d ≈ 0.05, arbitrage exit, institutional power) because it is the beneficiary—the constraint is designed to benefit this framing. Congress/parties experience moderate extraction (d ≈ 0.55, constrained exit, organized power) because they both benefit (advancement opportunities) and bear costs (constrained presidential leverage). The analytical observer at civilizational scope occupies the false-summit position where neutrality risks naturalizing the constraint; the structural data (beneficiaries, victims, extractiveness, suppression) reveal this is not a natural law but a chosen institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint is explicitly a snare from the voter's perspective (suppression of electoral option, no exit). The mandatrophy is resolved by recognizing that precommitment constitutionalism is a genuine beneficiary (not just a rhetorical framing) with verifiable interest in rotation norms and constitutional stability. The extraction is real: the voter's option is removed. The coordination benefit to precommitment constitutionalism is also real: the amendment protects against future democratic override of rotation norms. No single type resolves the constraint because the constraint distributes extraction and benefit asymmetrically across agents. The snare classification stands from the powerless/trapped voter perspective because extraction is irreversible and unsustainable through ordinary democratic means. The rope classification stands from the beneficiary perspective because coordination is achieved without coercion of the institutional actor. The false-summit mountain classification in the analytical observer perspective is detected through beneficiary/victim structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_override_feasibility,
    'Is the constitutional supermajority requirement (2/3 of Congress, 3/4 of states) itself an expression of democratic choice, or does it function as anti-democratic constraint on democratic choice?',
    'Recursive political theory: analyze whether supermajority requirements for amendment are themselves legitimate expressions of popular sovereignty or pre-emptive foreclosure of future democratic will. Compare with other constitutional democracies'' amendment procedures.',
    'If supermajority is legitimate: the suppression is contingent and revisable (snare → rope if viewed as coordination). If supermajority is itself anti-democratic: the suppression is doubly locked (snare is confirmed and deepened; no escape mechanism exists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_override_feasibility, conceptual, 'Whether supermajority amendment requirement is itself a legitimate expression of democratic choice').

omega_variable(
    precommitment_vs_distrust_reading,
    'Does the Twenty-Second Amendment express the people''s own precommitment to rotation, or does it express the framers'' distrust of the future demos'' capacity to choose wisely?',
    'Historical exegesis of ratification debates and post-WWII political context (reaction to Roosevelt''s four terms). Analyze whether amendment was adopted as popular constraint on tyranny risk or as elite constraint on popular choice.',
    'If precommitment: constraint is coordination mechanism (rope/tangled_rope from institutional perspectives). If distrust: constraint is extraction mechanism (snare from voter perspective). This reading is the distrust branch.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precommitment_vs_distrust_reading, conceptual, 'Whether amendment expresses popular precommitment or framers'' distrust of future voters').

omega_variable(
    presidential_incumbency_extraction_asymmetry,
    'Does the term limit extract value specifically from the incumbent''s re-election option, or does it suppress a general democratic option available to all?',
    'Comparative analysis: if term limits applied equally to governors, legislators, judges, would the suppression rate remain the same? Focus on whether extraction targets the presidency specifically or democratic choice generally.',
    'If presidency-specific: extractiveness may be lower (addresses specific tyranny risk). If general: extractiveness is high (affects all future ballot categories uniformly). This reading emphasizes the general suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_incumbency_extraction_asymmetry, empirical, 'Whether suppression targets the presidency specifically or democratic choice generally').

omega_variable(
    norm_internalization_durability,
    'How stable is the norm of voluntary compliance with the Twenty-Second Amendment if the constitutional text is never actively enforced by courts or elections officials?',
    'Historical precedent analysis: has any presidential candidate tested the boundary (attempted to run despite the amendment)? Psychological and political theory: how long can norms persist without enforcement? Model scenarios of institutional pressure changes.',
    'If norm is durable: piton classification is stable (theater-maintained constraint survives through internalization). If norm is fragile: piton masks underlying snare that would re-activate if cultural conditions shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_internalization_durability, empirical, 'Durability of voluntary compliance norm without active court enforcement').

omega_variable(
    reading_contest_identity,
    'What distinguishes THIS reading (democratic_choice_objection_reading) from its siblings (anti_caesarism_reading, lame_duck_cost_reading)?',
    'This is a committer-frame omega documenting the kernel contest. The democratic_choice_objection reading emphasizes permanent suppression of an electoral category as distrust of voters. The anti_caesarism reading emphasizes rotation as safeguard against executive tyranny. The lame_duck_cost reading emphasizes second-term disempowerment of the president. All three share the same constitutional text but read different structural consequences into it.',
    'The kernel contest determines how the amendment is classified and what omega variables apply. This reading produces snare classification (suppression of voter choice). The anti_caesarism reading would produce tangled_rope (coordination against tyranny risk with extraction cost). The lame_duck_cost reading would produce tangled_rope (coordination of accountability with loss of executive leverage).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_identity, conceptual, 'Identity and structural differentiation of this reading from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(twenty_second_amendment__democratic_choice_objection_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsa_dco_tr_t0, twenty_second_amendment__democratic_choice_objection_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tsa_dco_tr_t50, twenty_second_amendment__democratic_choice_objection_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(tsa_dco_be_t0, twenty_second_amendment__democratic_choice_objection_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tsa_dco_be_t25, twenty_second_amendment__democratic_choice_objection_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(tsa_dco_be_t50, twenty_second_amendment__democratic_choice_objection_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(tsa_dco_be_t70, twenty_second_amendment__democratic_choice_objection_reading, base_extractiveness, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(twenty_second_amendment__democratic_choice_objection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(twenty_second_amendment__democratic_choice_objection_reading, twenty_second_amendment__anti_caesarism_reading).
narrative_ontology:affects_constraint(twenty_second_amendment__democratic_choice_objection_reading, twenty_second_amendment__lame_duck_cost_reading).

% DUAL FORMULATION NOTE:
% The Twenty-Second Amendment instantiates THREE structurally distinct constraints corresponding to three readings of the kernel. This file instantiates the democratic_choice_objection_reading: suppression of electoral option as distrust of voters. The anti_caesarism_reading emphasizes rotation as institutional defense against tyranny (would classify as tangled_rope from several perspectives, with genuine coordination function). The lame_duck_cost_reading emphasizes second-term presidential disempowerment (would classify as tangled_rope, with extraction targeted at executive leverage rather than voter choice). All three readings share the constitutional text; they differ in structural consequence attribution. Each story has its own extractiveness value, beneficiary/victim structure, and perspective set. The readings coexist in constitutional discourse—multiple parties hold different readings simultaneously. This file's snare classification applies specifically to the democratic choice objection reading's structural analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
