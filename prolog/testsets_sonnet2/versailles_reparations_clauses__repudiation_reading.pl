% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses — Repudiation Reading (Diktat Thesis)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the repudiation reading of the contested
 *   Versailles reparations kernel: the position, articulated by Weimar-era
 *   German nationalists, revisionist historians, and later echoed in
 *   appeasement-era Anglo-American opinion, that the treaty's reparations
 *   clauses were procured under duress (blockade continuation,
 *   renewed-invasion threat, no negotiation) and are therefore void of
 *   binding moral or legal force beyond token, voluntarily renegotiated
 *   payments. This reading treats Article 231 not as a legal formality
 *   (limited_responsibility_reading) nor as grounding quasi-unlimited
 *   liability (punitive_liability_reading) but as itself the coerced
 *   instrument that should be nullified. The ε authored here is high because,
 *   BY THIS READING'S OWN LIGHTS, the standing arrangement under contest —
 *   the enforced reparations schedule as it actually operated through 1932 —
 *   is an almost total extraction structure backed by military occupation
 *   (the 1923 Ruhr crisis is the reading's central evidentiary exhibit). This
 *   is the referent for ε: the arrangement under contest as this reading sees
 *   it, not the reading's own preferred zero-obligation endpoint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.91).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.86).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses — Repudiation Reading (Diktat Thesis)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '6b919746-8ec3-4569-a165-9cd54ec49a17').
narrative_ontology:cs_kernel_codification('6b919746-8ec3-4569-a165-9cd54ec49a17', fixed_text).
narrative_ontology:cs_authority_grounding('6b919746-8ec3-4569-a165-9cd54ec49a17', extraction).
narrative_ontology:cs_interpretation_layer_present('6b919746-8ec3-4569-a165-9cd54ec49a17').
narrative_ontology:cs_reading_relation('6b919746-8ec3-4569-a165-9cd54ec49a17', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('6b919746-8ec3-4569-a165-9cd54ec49a17', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_axiom('6b919746-8ec3-4569-a165-9cd54ec49a17', foundational, duress_vitiates_treaty_obligation).
narrative_ontology:cs_axiom_status(duress_vitiates_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6b919746-8ec3-4569-a165-9cd54ec49a17', duress_vitiates_treaty_obligation, deontological).
narrative_ontology:cs_axiom('6b919746-8ec3-4569-a165-9cd54ec49a17', foundational, article_231_is_coerced_instrument_not_valid_liability_ground).
narrative_ontology:cs_axiom_status(article_231_is_coerced_instrument_not_valid_liability_ground, holdable).
narrative_ontology:cs_axiom_grounding('6b919746-8ec3-4569-a165-9cd54ec49a17', article_231_is_coerced_instrument_not_valid_liability_ground, conventional).
narrative_ontology:cs_reference_frame('6b919746-8ec3-4569-a165-9cd54ec49a17', sovereign_consent_as_treaty_precondition).
narrative_ontology:cs_drift_state('6b919746-8ec3-4569-a165-9cd54ec49a17', post_ruhr_occupation_1923, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('6b919746-8ec3-4569-a165-9cd54ec49a17', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, reparations_commission_administrators).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, french_ruhr_occupation_interests).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_industrial_workforce).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_currency_holders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, weimar_democratic_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_nationalist_and_revisionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed under threat of continued blockade and renewed invasion, with no seat at the drafting table and no right to negotiate terms. Bound to Article 231's war-guilt clause and to a reparations schedule set unilaterally by the victors, enforced by continued occupation of Rhineland territory and the standing threat of further sanctions. Exit would mean renewed war or total economic collapse under blockade; the reading holds the treaty void ab initio as a product of coercion rather than consent.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, payer,
    moderate, generational, trapped, national).

% Bears the material weight of reparations transfers through requisitioned coal, seized rail stock, and the hyperinflationary financing the state resorts to in order to meet in-kind delivery schedules. Has no channel to contest the terms and no exit from the national economy that must fund them.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_industrial_workforce, payer,
    powerless, biographical, trapped, national).

% Savings destroyed by the 1923 hyperinflation triggered substantially by the state printing money to meet reparations and cash indemnities and to fund passive resistance to the Ruhr occupation. Cannot exit the currency or the political consequences of the payment schedule.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_currency_holders, payer,
    powerless, immediate, trapped, national).

% The republic that signed and administered the treaty is discredited by association with it; nationalist and revanchist movements use the 'Diktat' framing to delegitimize the government itself, not merely the treaty. This is a non-agent casualty tracked for completeness, not a party that can act.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, weimar_democratic_legitimacy, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(versailles_reparations_clauses__repudiation_reading, weimar_democratic_legitimacy).

% France and Belgium in particular set the reparations schedule to fund reconstruction of war-devastated territory and to permanently cap German industrial and military capacity; Britain and the US collect indirectly through inter-allied war debt linkage. They hold drafting power, enforcement power (occupation, sanctions), and can revise terms unilaterally while Germany cannot.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments, beneficiary,
    institutional, generational, arbitrage, continental).

% Sets, audits, and enforces the payment schedule and in-kind delivery quotas under treaty authority; can declare Germany in default (as in 1923) and authorize occupation as an enforcement remedy. Administers the enforcement machinery the repudiation reading holds illegitimate.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, reparations_commission_administrators, agenda_setter,
    institutional, generational, analytical, continental).

% French and Belgian troops occupy the Ruhr industrial region in 1923 to seize coal and coke directly after Germany defaults, extracting resources at the point of a bayonet. This is the enforcement action the repudiation reading identifies as proof the arrangement is a coerced extraction structure rather than a legitimate legal obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_ruhr_occupation_interests, beneficiary,
    powerful, biographical, mobile, regional).

% Uses the 'War Guilt Lie' and 'Diktat' framing to mobilize political support, gaining domestically from the very grievance the repudiation reading formalizes into legal argument; benefits from repudiation independent of whether the legal claim is sound.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_nationalist_and_revisionist_movements, beneficiary,
    organized, generational, mobile, national).

% Has no voice in the reading's own moment: the repudiation reading, by nullifying reparations and security guarantees together, removes the disarmament and demilitarization constraints on Germany without those constraints' downstream stabilizing function ever being represented in the argument. Tracked as a non-agent casualty of the reading's structural implications.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, future_european_security_order, excluded,
    powerless, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(versailles_reparations_clauses__repudiation_reading, future_european_security_order).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized by this reading: the repudiation reading holds that the treaty performs no legitimate coordination function because consent — the precondition for treaty legitimacy — was absent. What the arrangement's defenders call coordination (reconstruction financing, war-cost allocation) this reading treats as extraction dressed in legal form.
% TRANSFER_FUNCTION: As enforced, the arrangement moves industrial output, currency stability, and territorial sovereignty from Germany to the Allied creditor governments and their administrative apparatus; the repudiation reading holds this transfer has no valid legal basis and should be reversed to zero beyond token, voluntarily-negotiated gestures.
% ABSENT_VOICES: German negotiators were excluded from the actual Paris Peace Conference drafting sessions and presented the finished text on a take-it-or-leave-it basis under blockade threat — the reading's central grievance is precisely this exclusion. Conversely, the reading itself excludes Belgian and French civilian reconstruction claimants, whose devastated departments (mines, farmland, rail lines) find no voice within the repudiation frame's totalizing rejection.
% DISAPPEARANCE_RATIONALE: If the reparations clauses were nullified as this reading demands, French and Belgian reconstruction financing collapses, the Ruhr occupation loses its legal pretext, German industrial capacity and currency policy are freed from treaty oversight, and the entire inter-allied war-debt settlement structure (which depended on German payments flowing through to London and Washington) requires renegotiation — the interwar financial and security order visibly reorganizes.
% FOUNDING_PROBLEM: The Allied powers built the reparations regime to fund reconstruction of war-devastated territory, assign moral and financial responsibility for a war of unprecedented destruction, and cap Germany's capacity to rearm.
% FOUNDING_PROBLEM_CORROBORATION: The repudiation reading holds the founding problem was never a legitimate coordination problem at all but a punitive fiction imposed under duress — an interpretation corroborated externally by contemporaneous critics including John Maynard Keynes (The Economic Consequences of the Peace, 1919, writing from within the British delegation but publicly breaking with it) and by the Allied powers' own subsequent partial retreat (Dawes Plan 1924, Young Plan 1929, Hoover Moratorium 1931, Lausanne Conference 1932 effectively ending payments) — none of which are voices of the German beneficiary side.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.91, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply around the 1923 Ruhr occupation (0.93) when France and Belgium physically seized Rhineland coal and coke after a German default, then eases somewhat under the Dawes (1924) and Young (1929) renegotiations before the reading marks a final spike in 1933 as Hitler's regime uses continued Allied claims (formally still open until Lausanne 1932/33) as propaganda proof of ongoing coercion. Suppression tracks military and financial enforcement capacity — highest during active occupation, lower during diplomatic renegotiation phases. Theater ratio rises over the interval as the underlying payment mechanism increasingly becomes a diplomatic performance (serial renegotiation, moratoria) rather than functioning transfer, consistent with the reading's claim that the arrangement persisted more as a legitimating fiction than a live transfer scheme by the late 1920s.
 *
 * DIRECTIONALITY LOGIC:
 *   German state, workforce, and currency holders are structural targets: they bear the schedule, cannot renegotiate unilaterally, and face military enforcement (Ruhr) if they default — d sits near the full-target end for all three. Allied creditor governments and the Reparations Commission are structural beneficiaries and agenda-setters: they draft, administer, revise, and enforce the schedule from outside its cost structure — d sits near the full-beneficiary end. French Ruhr occupation interests are a beneficiary subset with the most direct, physical capture of the transfer. German nationalist movements are an unusual beneficiary class: they gain politically from the grievance itself, independent of whether repudiation succeeds, which the reading treats as corroborating evidence of the arrangement's illegitimacy rather than as a complicating factor.
 *
 * MANDATROPHY ANALYSIS:
 *   The repudiation reading's mandatrophy claim is that the founding problem (funding reconstruction, capping German power) was real in 1919 but the enforcement mechanism outlived any legitimate claim to it once it required military occupation of sovereign territory to function (1923) — at that point the arrangement's own creators (see Dawes, Young, Hoover, Lausanne) began retreating from it, which this reading reads as tacit admission the obligation was never as sound as Article 231's language claimed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_vitiates_consent_threshold,
    'Does the coercive context of Versailles''s drafting and signing (blockade continuation, no German negotiating seat, threatened resumption of war) meet the threshold under which treaty obligations are voided by duress, as opposed to being a hard but legally binding negotiated settlement typical of war termination?',
    'Comparative international-law analysis against later-codified duress doctrine (e.g., Vienna Convention on the Law of Treaties Art. 52, adopted 1969, not retroactive to 1919) and against the historical pattern of prior war-termination treaties (e.g., Frankfurt 1871) to determine whether Versailles''s coercive features were qualitatively different or merely a standard feature of imposed peace settlements.',
    'If Versailles is found structurally comparable to other imposed peace treaties historically treated as binding, the repudiation reading''s foreclosure of any obligation weakens toward the limited_responsibility reading; if found qualitatively more coercive (unprecedented blockade continuation post-armistice, total exclusion from drafting), the repudiation reading''s total-nullification claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_vitiates_consent_threshold, conceptual, 'Whether Versailles''s coercive drafting conditions meet a genuine duress-vitiates-consent threshold or reflect ordinary imposed-peace severity.').

omega_variable(
    repudiation_reading_selection_effect,
    'Is the repudiation reading selected here because it best fits the documentary record of 1919–1933, or because it is the reading most useful to the political movements (German nationalist, later National Socialist) that promoted it most vigorously?',
    'Trace the reading''s adoption independent of its beneficiary political movements — e.g., Keynes''s 1919 critique from within the British delegation, and later moderate Anglo-American revisionism (e.g., the Dawes/Young plan architects'' own acknowledgment of unsustainability) — versus its adoption and amplification specifically by Weimar nationalist and Nazi propaganda after 1930.',
    'If the reading''s strongest corroboration comes from disinterested contemporaneous economists rather than from the beneficiary political movements, it is better evidenced independent of its later political misuse; if it primarily gained force through Nazi propaganda deployment, its evidentiary standing (independent of its later use) should be assessed separately from that deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repudiation_reading_selection_effect, conceptual, 'Whether the repudiation reading''s credibility should be assessed independent of the political movements that most benefited from adopting it.').

omega_variable(
    security_nullification_downstream_cost,
    'This reading''s structural delta nullifies not only payment obligations but the associated Allied security guarantees and disarmament framework. Is that nullification a necessary logical consequence of the duress argument, or a separable policy choice being smuggled in alongside the reparations repudiation?',
    'Legal-doctrinal analysis of whether duress voiding one treaty provision (reparations) logically requires voiding structurally distinct provisions (military limitation clauses, territorial arrangements) negotiated in the same instrument, versus severability doctrine that would allow partial nullification.',
    'If severability applies, the repudiation reading''s rejection of reparations need not extend to nullifying the security architecture — a materially different (less destabilizing) reading than the one authored here. If the clauses are held inseverable, the full structural delta (including security nullification) follows from the duress argument as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_nullification_downstream_cost, conceptual, 'Whether nullifying reparations obligations under a duress theory logically entails nullifying the treaty''s separate security and disarmament provisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1921, 0.28).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1925, 0.42).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.5).
narrative_ontology:measurement(vers_tr_t1933, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1933, 0.4).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.72).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1921, 0.8).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.93).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1925, 0.85).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.7).
narrative_ontology:measurement(vers_be_t1933, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1933, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.75).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1921, 0.78).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.95).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(vers_su_t1933, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1933, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the versailles_reparations_clauses kernel, each instantiating a structurally distinct constraint with its own ε: repudiation_reading (this file, ε=0.91, snare) holds the arrangement wholly illegitimate; limited_responsibility_reading holds it a capacity-bounded obligation; punitive_liability_reading holds it a near-unlimited moral-financial liability. The three share the same treaty text and the same historical episodes (Ruhr 1923, Dawes 1924, Young 1929) but assign radically different legitimacy and extraction values to the same events because they differ on whether consent was vitiated by duress — that is the located disagreement, per the ε-invariance principle these are not one constraint measured three ways but three distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
