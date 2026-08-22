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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Versailles Reparations Clauses — Repudiation Reading (Diktat Illegitimacy)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the repudiation reading of the Versailles
 *   reparations kernel: the claim that the treaty was extracted from Germany
 *   under duress (continued Allied blockade, threat of renewed invasion,
 *   denial of negotiating standing) and is therefore void of binding moral or
 *   legal force beyond token acknowledgment. This is NOT the
 *   punitive_liability_reading (which holds Germany bears unique,
 *   quasi-unlimited responsibility) nor the limited_responsibility_reading
 *   (which treats Article 231 as legal formality bounding payments to
 *   capacity). Under the repudiation reading, ε is high because the reading
 *   holds that the entire extraction apparatus — Commission assessments, Ruhr
 *   occupation, currency pressure — operates against a debtor who never
 *   validly consented, making the standing arrangement (payment enforcement
 *   under the 1919 treaty) maximally extractive by this reading's own lights.
 *   The referent for ε is that standing enforcement arrangement, not the
 *   reading's endorsed alternative (near-zero German obligation) — per the
 *   ε-referent rule for kernel readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.91).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.93).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses — Repudiation Reading (Diktat Illegitimacy)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '6a08ca9b-904b-4c28-a574-90f4b7c4bb57').
narrative_ontology:cs_kernel_codification('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', fixed_text).
narrative_ontology:cs_authority_grounding('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', extraction).
narrative_ontology:cs_interpretation_layer_present('6a08ca9b-904b-4c28-a574-90f4b7c4bb57').
narrative_ontology:cs_reading_relation('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', foundational, duress_voids_treaty_obligation).
narrative_ontology:cs_axiom_status(duress_voids_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', duress_voids_treaty_obligation, deontological).
narrative_ontology:cs_axiom('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', foundational, article_231_signature_not_valid_consent).
narrative_ontology:cs_axiom_status(article_231_signature_not_valid_consent, holdable).
narrative_ontology:cs_axiom_grounding('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', article_231_signature_not_valid_consent, conventional).
narrative_ontology:cs_reference_frame('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', coerced_signature_void_ab_initio).
narrative_ontology:cs_drift_state('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', post_lausanne_1932, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('6a08ca9b-904b-4c28-a574-90f4b7c4bb57', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_reparations_commission).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, french_treasury).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, belgian_treasury).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, british_treasury).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_working_population).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_currency_holders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_industrial_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, united_states_treasury).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, consent_based_treaty_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, rebus_sic_stantibus_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the schedule of payments, assesses German capacity, and holds enforcement levers including occupation of the Ruhr. Under this reading, the Commission is not a neutral creditor body but the enforcement arm of a coerced settlement — it collects under threat, not agreement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_reparations_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receives the largest share of reparations receipts and uses continued extraction to finance reconstruction and war-debt service to the United States. Under this reading, France benefits from a claim that was never validly consented to by the debtor and enforces it through occupation and sanction rather than through a legitimate obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_treasury, beneficiary,
    institutional, generational, arbitrage, national).

% Receives a smaller reparations share tied to wartime devastation claims. Benefits from the schedule but has less independent enforcement capacity than France or Britain, remaining dependent on Allied consensus to sustain collection.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, belgian_treasury, beneficiary,
    powerful, generational, constrained, national).

% Collects a reparations share but by the mid-1920s increasingly advocates downward revision (Dawes, Young Plans) out of concern the schedule destabilizes European trade and its own recovery. Structurally a beneficiary but with exit options the French lack — Britain can and does revise its position.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, british_treasury, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, british_treasury, observer).

% Signed the treaty under threat of renewed invasion and continued blockade, with the war-guilt clause (Article 231) presented as a precondition rather than a negotiated finding. Under this reading, the state's formal signature does not constitute valid consent; it is treated as bound to payments it argues were never legitimately assumed, while occupation and sanction remain live threats against default.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, payer,
    powerful, generational, trapped, national).

% Bears the reparations burden through taxation, currency depreciation, and the hyperinflation of 1921-1923 that this reading treats as a direct consequence of the extraction schedule. Has no voice in the treaty's terms and no exit from the national obligation imposed on them.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_working_population, payer,
    powerless, biographical, trapped, national).

% Savings and wages are wiped out as the state monetizes debt and prints currency partly to meet reparations demands. Under this reading, this population absorbs the cost of an obligation the reading holds should never have attached to Germany in the first place.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_currency_holders, payer,
    powerless, immediate, trapped, national).

% Subject to in-kind reparations transfers (coal, machinery, rail stock) and to the 1923 Ruhr occupation triggered by default on coal deliveries. Some industrial actors have limited capacity to relocate or restructure around the extraction but remain bound by treaty enforcement mechanisms.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_industrial_sector, payer,
    moderate, biographical, constrained, national).

% Nationalist and revisionist political factions within Germany who argue the treaty is a diktat with no binding force, but who were not party to its negotiation and whose objections carry no standing in the Commission's enforcement process. Their exclusion from the 1919 negotiations is precisely what this reading treats as delegitimizing the outcome.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, weimar_domestic_opposition, excluded,
    organized, biographical, trapped, national).

% Not a reparations recipient under the treaty but a war-debt creditor to Britain and France, whose loans are serviced indirectly by reparations receipts (the Dawes/Young circular-flow arrangement). Observes and periodically intervenes to restructure the schedule when it threatens the broader debt architecture it depends on.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, united_states_treasury, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, united_states_treasury, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, diffuse).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading concedes no genuine coordination function survives the duress finding: what the treaty's proponents frame as an internationally coordinated post-war settlement, this reading treats as coerced signature extracted under threat of continued blockade and invasion, with the 'coordination' being the coordination of creditors against a debtor denied a negotiating position.
% TRANSFER_FUNCTION: Moves specie, in-kind industrial goods, and currency value from the German state, its working population, and its industrial sector to the Allied treasuries (chiefly France, with shares to Belgium and Britain), enforced through occupation (the 1923 Ruhr action) and the threat of renewed military occupation.
% ABSENT_VOICES: German negotiators were excluded from the 1919 Paris deliberations that set the terms they were then required to sign — the treaty was presented for signature, not negotiated with German input. Domestic Weimar political factions who held this repudiation position from the outset had no standing before the Reparations Commission.
% DISAPPEARANCE_RATIONALE: Under this reading, if the reparations obligation were formally nullified, German fiscal policy would no longer be structured around debt service to the Commission, the Ruhr occupation rationale disappears, German rearmament constraints tied to reparations-linked treaty enforcement loosen, and the currency-debasement dynamic driving hyperinflation loses its principal external driver — a materially different interwar trajectory follows.
% FOUNDING_PROBLEM: The Allied powers sought compensation for wartime destruction and a mechanism to prevent German remilitarization by keeping its economy structurally weakened and financially obligated to its former adversaries.
% FOUNDING_PROBLEM_CORROBORATION: British Treasury officials (e.g., Keynes's own 1919 dissent and resignation from the British delegation) attest from outside the German beneficiary-adjacent position that the schedule exceeded German capacity and was economically counterproductive even by Allied recovery standards — this is corroboration from within the creditor camp itself, not solely from German sources, that the founding problem as designed was already contested at signing.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises through 1923 (Ruhr occupation, hyperinflation peak) reflecting escalating enforcement against a treaty this reading treats as already illegitimate at signing; the partial retreat at 1925-1929 (Dawes/Young revisions) reflects Allied recognition of unsustainable extraction without conceding the duress premise, hence extraction rebounds by 1932 as Depression-era default pressure returns. Theater ratio rises over the interval as the Commission's assessment machinery increasingly performs technical capacity-review functions (Dawes Plan expert committees) that this reading treats as legitimizing cover for a claim it holds has no underlying legal basis. Suppression peaks with the 1923 Ruhr occupation (direct military enforcement) and declines as occupation becomes politically costly for France and Belgium.
 *
 * DIRECTIONALITY LOGIC:
 *   German state, working population, currency holders, and industrial sector are victims: the reading holds none of them meaningfully consented to the obligation, and all bear costs through taxation, inflation, or in-kind transfer under continuing threat of enforcement. Allied treasuries are beneficiaries collecting under a claim this reading treats as void. Britain is differentiated from France by mobile exit (revisionist policy shift, Dawes/Young sponsorship) versus France's more rigid enforcement posture — same nominal institutional power, different structural exit given differing debt-service dependence on reparations receipts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing German remilitarization, compensating wartime destruction) is treated by this reading as either never legitimately established (duress vitiates the underlying consent) or as having outlived any defensible bounds by the mid-1920s — hence founding_problem_status is authored as contested rather than flatly dead, since Allied creditor treasuries continued to assert the problem's liveness while British Treasury dissent (Keynes) and eventual multilateral revision (Dawes, Young, Lausanne 1932 effective cancellation) corroborate from outside the German position that the arrangement had exceeded any coordination function it once had.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_vitiates_consent_threshold,
    'Does the coercive context of the 1919 signature (continued blockade, threatened resumption of invasion, exclusion from negotiation) meet the legal/moral threshold for duress sufficient to void treaty obligations, or is this the ordinary condition of any imposed post-war settlement and therefore not disqualifying?',
    'Comparative analysis against other imposed post-war settlements (1871 Frankfurt, 1945 unconditional surrenders) to establish whether duress-under-military-defeat is a recognized voiding condition in state practice or customary international law, versus a standard feature of all such treaties.',
    'If duress is found to meet a recognized voiding threshold, the repudiation reading''s core premise is vindicated as a matter of legal doctrine, not merely political grievance. If not, the reading survives only as a normative/political claim rather than a legal one, which would not change ε (authored from the reading''s own lights) but would affect how the reading''s claims are weighted against the siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_vitiates_consent_threshold, conceptual, 'Whether the coercive signing context meets a recognized threshold for treaty invalidity.').

omega_variable(
    capacity_versus_legitimacy_conflation,
    'Is the repudiation reading''s rejection of the entire obligation actually a claim about legitimacy (the treaty was never validly binding) or is it substantively a capacity claim (Germany could not pay the scheduled amounts) dressed in legitimacy language?',
    'Examine whether repudiation-reading advocates would accept a reduced, capacity-bounded schedule if legitimacy were independently established (i.e., do they converge with limited_responsibility_reading once magnitude is fixed, or do they reject any payment regardless of amount).',
    'If repudiation collapses into a capacity argument at the margin, it is less structurally distinct from limited_responsibility_reading than the kernel decomposition assumes, which would bear on whether the two are properly separate constraints or points on a continuum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_versus_legitimacy_conflation, conceptual, 'Whether repudiation is a distinct legitimacy claim or a capacity claim in different language.').

omega_variable(
    post_1932_settlement_as_corroboration,
    'Does the effective cancellation of reparations at the 1932 Lausanne Conference corroborate the repudiation reading''s premise, or does it merely reflect Depression-era practical abandonment without conceding the illegitimacy claim?',
    'Review Lausanne Conference documentary record for whether cancellation was framed by Allied parties as recognition of infeasibility (capacity) versus recognition of any legitimacy defect in the original claim.',
    'If Lausanne framing was purely practical, the repudiation reading''s legitimacy claim remains contested rather than externally validated even by the arrangement''s own effective end; if framed as a legitimacy concession, it strengthens the founding_problem_status corroboration outside the German seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_1932_settlement_as_corroboration, empirical, 'Whether the 1932 cancellation validates the duress/illegitimacy claim or merely reflects practical abandonment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1921, 0.28).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1925, 0.4).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.45).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1932, 0.42).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.7).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1921, 0.82).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.93).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1925, 0.88).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.79).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1932, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.75).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1921, 0.8).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.95).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1925, 0.85).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.7).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1932, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__repudiation_reading, 0.05).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, weimar_hyperinflation_dynamics).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, ruhr_occupation_enforcement).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the versailles_reparations_clauses kernel, decomposed per the ε-invariance principle: the punitive_liability_reading treats Article 231 as grounding quasi-unlimited claims (low-to-moderate ε from that reading's own lights, since the extraction is framed as owed), the limited_responsibility_reading treats the same clause as a bounded legal formality (moderate ε, capacity-constrained), and this repudiation_reading treats the entire arrangement as coerced and void (high ε, near-total suppression of creditor claims by the reading's own accounting). All three share the same textual kernel (the Treaty of Versailles reparations articles) but instantiate structurally distinct constraints with different beneficiary/victim framings and different ε values — they are linked here rather than merged into one story with a variable observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
