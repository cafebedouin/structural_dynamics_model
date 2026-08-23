% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: ICC Complementarity Principle (International Oversight Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   The Rome Statute's Article 17 complementarity principle establishes that
 *   the ICC may only prosecute when national jurisdictions are 'unwilling or
 *   unable genuinely' to do so. The international_oversight_reading
 *   interprets this threshold broadly: complementarity is an accountability
 *   trigger, not a sovereignty shield. The ICC intervenes when domestic
 *   proceedings lack independence, genuine intent, or cover victor's justice
 *   and elite immunity. Beneficiaries are victims in states where power
 *   structures block justice; the victim set expands to include scenarios
 *   where states stage symbolic or sham prosecutions. State cooperation
 *   demands intensify — non-cooperation triggers Article 87 referrals and
 *   political consequences. This reading sits in tension with the
 *   national_primacy_reading, which treats national courts as presumptively
 *   adequate and places the burden on the ICC to prove inadmissibility.
 *
 * KEY AGENTS:
 *   - ICC (agenda_setter/institutional) — administers complementarity regime, decides admissibility
 *   - Victims in complicit/failed states (beneficiary/powerless) — gain access to justice when states fail
 *   - States shielding elites (payer/powerful) — bear sovereignty costs when ICC intervenes
 *   - Non-cooperating states parties (payer/organized) — face enforcement pressure for non-compliance
 *   - Powerful non-party states (excluded/powerful) — US, China, Russia; immune but shape discourse
 *   - International legal community (observer/analytical) — interprets, critiques, legitimates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.55).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.45).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "ICC Complementarity Principle (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '2944fb63-127d-4db0-9cad-9270ab86c163').
narrative_ontology:cs_kernel_codification('2944fb63-127d-4db0-9cad-9270ab86c163', formalized).
narrative_ontology:cs_authority_grounding('2944fb63-127d-4db0-9cad-9270ab86c163', extraction).
narrative_ontology:cs_interpretation_layer_present('2944fb63-127d-4db0-9cad-9270ab86c163').
narrative_ontology:cs_reading_relation('2944fb63-127d-4db0-9cad-9270ab86c163', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('2944fb63-127d-4db0-9cad-9270ab86c163', foundational, complementarity_serves_anti_impunity).
narrative_ontology:cs_axiom_status(complementarity_serves_anti_impunity, holdable).
narrative_ontology:cs_axiom_grounding('2944fb63-127d-4db0-9cad-9270ab86c163', complementarity_serves_anti_impunity, deontological).
narrative_ontology:cs_axiom('2944fb63-127d-4db0-9cad-9270ab86c163', secondary, genuine_proceedings_require_independence).
narrative_ontology:cs_axiom_status(genuine_proceedings_require_independence, holdable).
narrative_ontology:cs_axiom_grounding('2944fb63-127d-4db0-9cad-9270ab86c163', genuine_proceedings_require_independence, conventional).
narrative_ontology:cs_reference_frame('2944fb63-127d-4db0-9cad-9270ab86c163', rome_statute_originalist_complementarity).
narrative_ontology:cs_drift_state('2944fb63-127d-4db0-9cad-9270ab86c163', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2944fb63-127d-4db0-9cad-9270ab86c163', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, affected_communities_in_failed_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_prosecutors).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, states_shielding_elites).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, non_cooperating_states_parties).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, states_facing_victors_justice_prosecutions).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, anti_impunity_norm).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, complementarity_as_accountability_trigger).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, genuine_proceedings_require_independence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the complementarity regime: decides admissibility, assesses 'genuineness' of domestic proceedings, requests state cooperation. Gains institutional authority and legitimacy from exercising oversight. Structurally positioned to expand jurisdiction through interpretation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_chambers_prosecutor, agenda_setter,
    institutional, generational, analytical, global).

% Victims of international crimes in states where power structures (military, government, elites) block domestic justice. The broad complementarity interpretation is their only path to prosecution. No exit — they cannot leave the jurisdiction or access alternative courts. Gains are justice access; costs are zero.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, local).

% Communities in states with collapsed judicial systems (unable, not unwilling). ICC intervention fills the vacuum. They benefit from any prosecution; no capacity to influence the constraint. Gains are accountability; costs are zero.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, affected_communities_in_failed_states, beneficiary,
    powerless, biographical, trapped, local).

% ICC prosecutors and staff whose professional mandate and institutional relevance depend on active complementarity jurisprudence. Benefit from low admissibility thresholds (more cases, more institutional purpose). Mobile exit — can move to other international tribunals or domestic roles.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_prosecutors, beneficiary,
    organized, biographical, mobile, global).

% States where political/military elites commit crimes and control domestic justice to ensure impunity (victor's justice, elite immunity). Broad complementarity triggers ICC intervention, stripping prosecutorial primacy. Exit is constrained — can withdraw from Rome Statute (political cost) or stage sham proceedings (theater cost). Bears sovereignty extraction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, states_shielding_elites, payer,
    powerful, biographical, constrained, national).

% Rome Statute parties that refuse cooperation (arrest, evidence, witness access) when ICC investigates their officials or allies. Face Article 87 referrals to ASP, political pressure, reputational costs. Exit constrained — withdrawal takes a year and doesn't undo pending obligations. Bears enforcement extraction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, non_cooperating_states_parties, payer,
    organized, biographical, constrained, national).

% Post-conflict states where new governments prosecute former regime figures in proceedings that may lack independence (victor's justice). Broad complementarity may deem these 'unwilling' and trigger ICC oversight. Exit constrained — cannot easily reform judiciary mid-conflict. Bears risk of ICC intervention on top of domestic transition costs.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, states_facing_victors_justice_prosecutions, payer,
    moderate, biographical, constrained, national).

% US, China, Russia, India, Israel — non-parties to Rome Statute. Structurally immune from complementarity but politically shape its interpretation through Security Council referrals (or blocks), Article 98 agreements, and funding leverage. Would object to broad interpretation as sovereignty threat but are not in the compliance conversation. Arbitrage exit — they choose engagement level.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, powerful_non_party_states, excluded,
    powerful, generational, arbitrage, global).

% Scholars, NGOs, judges, diplomats who interpret, critique, and legitimate complementarity jurisprudence. Neither collect nor pay; they produce the interpretive discourse that determines which reading prevails. Analytical exit — they observe from outside the constraint's direct operation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_legal_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that international crimes (genocide, crimes against humanity, war crimes) are prosecuted somewhere — either by states with genuine proceedings or by the ICC as court of last resort. Solves the impunity gap when states are unwilling or unable.
% TRANSFER_FUNCTION: Moves prosecutorial primacy from states that fail the 'genuine proceedings' test to the ICC. Transfers sovereignty prerogative (who prosecutes) from non-compliant states to an international court. Transfers justice access from zero (impunity) to non-zero (ICC prosecution) for victims in complicit/failed states.
% ABSENT_VOICES: Victims in non-party states (Syria, Myanmar, Yemen situations without Security Council referral) — would object to immunity of perpetrators but have no ICC access. Future generations — would bear consequences of normalization of impunity but are not represented. Domestic judges in targeted states — would claim professional competence but are assessed as 'not genuine' by ICC chambers.
% DISAPPEARANCE_RATIONALE: If broad complementarity vanished overnight, states shielding elites would face no ICC backstop; victor's justice proceedings would proceed without international scrutiny; victims in complicit/failed states would lose their only path to justice; the anti-impunity norm would degrade to aspirational rhetoric. The Rome Statute system would revert to a purely consensual regime with no trigger mechanism.
% FOUNDING_PROBLEM: Post-Cold War atrocities (Yugoslavia, Rwanda) revealed that sovereign states could not or would not prosecute their own leaders for international crimes. The ICTY/ICTR ad hoc tribunals were temporary; a permanent court with a complementarity trigger was created to ensure 'never again' without displacing functional national systems.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and victims' advocates (Human Rights Watch, Amnesty International, FIDH) attest the problem is live — cite ongoing impunity in Syria, Myanmar, Palestine, Ukraine (pre-2022), and elite immunity in African and Latin American states. Sovereignty-oriented states (African Union non-cooperation decisions, US ASPA, China/Russia positions) and some legal scholars (Klabbers, Simpson) attest the problem is substantially solved by improved national capacity and the mechanism now overreaches. No consensus; the contest is structural.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the sovereignty cost imposed on states that fail the genuine proceedings test — they lose prosecutorial primacy and face ICC intervention. Suppression (0.45) is moderate: Rome Statute parties are legally bound, but non-parties and powerful states evade; enforcement depends on state cooperation. Theater ratio (0.25) is low-moderate: ICC interventions are real but selective; some domestic proceedings are performative (sham trials to block admissibility). Accessibility collapse (0.55) is moderate: genuine national prosecutions remain available but collapse when political will is absent. Resistance (0.75) is high: powerful states resist via non-ratification, Article 98 agreements, Security Council leverage, and political non-cooperation. The claimed type is tangled_rope: genuine coordination (ending impunity gaps) + asymmetric extraction (sovereignty costs on non-compliant states) + active enforcement (ICC chambers, prosecutor, ASP).
 *
 * PERSPECTIVAL GAP:
 *   From the ICC/victim seat, this is a rope-like coordination mechanism that activates when states fail — genuine function, minimal extraction. From the state-shielding-elites seat, this is a snare — sovereignty extracted under a coordination cover, enforced by an unelected court. From powerful non-party states, it's a mountain they don't recognize — irrelevant natural law. The engine computes these per-seat divergences from the structural data; the claimed type (tangled_rope) reflects the structural reality that both coordination and extraction are real and simultaneous.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC (agenda_setter) sits near the beneficiary end (d ~ 0.15) — it gains authority and legitimacy from exercising complementarity. Victims in complicit states (beneficiary, powerless) are full beneficiaries (d ~ 0.05) — the constraint exists for them. States shielding elites (payer, powerful) are targets (d ~ 0.85) — they bear sovereignty extraction. Non-cooperating states parties (payer, organized) are targets (d ~ 0.75) — they face enforcement costs. Powerful non-party states (excluded, powerful) sit near analytical (d ~ 0.5) — they are structurally outside but politically affected. The broad interpretation shifts directionality toward victims by lowering the admissibility threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — impunity for international crimes when states are unwilling or unable — remains live (founding_problem_status: contested). The oversight reading argues the problem persists and the mechanism must remain low-threshold. The primacy reading argues the problem is solved by improved national capacity and the mechanism now overreaches. Mandatrophy is not resolved; the constraint's mandate is contested, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    broad_interpretation_legitimacy,
    'Does the broad ''unwilling or unable'' interpretation reflect the Rome Statute''s object and purpose, or does it exceed the treaty''s negotiated compromise on sovereignty?',
    'Travaux préparatoires analysis; ICJ advisory opinion; state practice convergence/divergence on admissibility challenges.',
    'If legitimate, the oversight reading is the correct treaty interpretation and national_primacy_reading is a restrictive deviation. If illegitimate, the oversight reading is judicial activism that extracts sovereignty beyond consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broad_interpretation_legitimacy, conceptual, 'Whether the broad admissibility threshold is a valid treaty interpretation or sovereignty overreach.').

omega_variable(
    icc_deterrence_effect,
    'Does the threat of ICC intervention under broad complementarity actually deter elite immunity and victor''s justice, or does it merely create performative domestic proceedings?',
    'Empirical study of domestic prosecution patterns before/after ICC preliminary examinations; comparison of sham vs. genuine proceedings in situation countries.',
    'If deterrence works, the coordination function is genuine and extraction is justified overhead. If only performative compliance results, the constraint is a theater-heavy tangled_rope trending toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(icc_deterrence_effect, empirical, 'Whether the accountability trigger actually prevents impunity or generates theatrical compliance.').

omega_variable(
    power_asymmetry_in_enforcement,
    'Does the complementarity mechanism apply symmetrically, or does it disproportionately target weak states while powerful non-party states (US, China, Russia) remain immune?',
    'Case distribution analysis: ratio of situations in party vs. non-party states; Security Council referral patterns; Article 98 agreement prevalence.',
    'If highly asymmetric, the constraint operates as a snare on weak states while powerful states free-ride — the tangled_rope classification masks a structural snare dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_in_enforcement, empirical, 'Whether the constraint''s enforcement falls disproportionately on powerless states.').

omega_variable(
    kernel_reading_boundary,
    'Is the international_oversight_reading a distinct constraint from the national_primacy_reading, or are they observably different applications of the same structural constraint?',
    'Compare ε values and stakeholder structures across the two readings; if ε differs materially, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story (current approach). If same constraint, the decomposition violates ε-invariance and should be merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel decomposition into two readings satisfies the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_oversight_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(art17_oversight_tr_t2007, article_17_complementarity__international_oversight_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(art17_oversight_tr_t2012, article_17_complementarity__international_oversight_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(art17_oversight_tr_t2017, article_17_complementarity__international_oversight_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(art17_oversight_tr_t2022, article_17_complementarity__international_oversight_reading, theater_ratio, 2022, 0.25).

% Extraction over time
narrative_ontology:measurement(art17_oversight_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.35).
narrative_ontology:measurement(art17_oversight_be_t2007, article_17_complementarity__international_oversight_reading, base_extractiveness, 2007, 0.42).
narrative_ontology:measurement(art17_oversight_be_t2012, article_17_complementarity__international_oversight_reading, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement(art17_oversight_be_t2017, article_17_complementarity__international_oversight_reading, base_extractiveness, 2017, 0.52).
narrative_ontology:measurement(art17_oversight_be_t2022, article_17_complementarity__international_oversight_reading, base_extractiveness, 2022, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(art17_oversight_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.3).
narrative_ontology:measurement(art17_oversight_su_t2007, article_17_complementarity__international_oversight_reading, suppression_requirement, 2007, 0.38).
narrative_ontology:measurement(art17_oversight_su_t2012, article_17_complementarity__international_oversight_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement(art17_oversight_su_t2017, article_17_complementarity__international_oversight_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(art17_oversight_su_t2022, article_17_complementarity__international_oversight_reading, suppression_requirement, 2022, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__international_oversight_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, rome_statute_cooperation_regime).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, state_sovereignty_norm).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, universal_jurisdiction_principle).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint and article_17_complementarity__national_primacy_reading form a constraint family decomposing the article_17_complementarity kernel. This reading (oversight) has higher extractiveness (0.55 vs ~0.30) because it treats more state conduct as triggering ICC jurisdiction. The sibling reading has lower extractiveness but higher suppression for victims (denied ICC access). They are linked by network.affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, powerful, 0.85).
constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, organized, 0.75).
constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, powerless, 0.05).
constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
