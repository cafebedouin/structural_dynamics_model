% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualist Method Discipline (Manifesto Revolutionary-Method Kernel)
 *   domain: political philosophy / revolutionary theory / historical materialism
 *
 * SUMMARY:
 *   This story models the democratic-gradualist method as an operative
 *   discipline on working-class politics: the standing arrangement in which
 *   the movement's power may be exercised only through electoral majorities
 *   and gradual institutional reform within the existing liberal-democratic
 *   state, with extra-parliamentary initiative defined as irresponsible and
 *   handled accordingly. The arrangement has a genuine coordination function
 *   — it aggregates millions into a legible political actor, wins real
 *   concessions, and avoids civil war — and a real extraction side:
 *   transformation is deferred indefinitely, the militant and council wings
 *   are suppressed as adventurist, and the party-union apparatuses whose
 *   positions depend on the method persisting capture the movement's energy.
 *   Claimed type is tangled_rope; the metrics are authored independently as
 *   descriptively true of the arrangement's actual operation. This file is
 *   one member of a three-story family decomposing the Manifesto's method
 *   kernel (see network.dual_formulation_note); the contest between readings
 *   is routed to kernel_context and omegas, not adjudicated here.
 *
 * KEY AGENTS:
 *   - social_democratic_parties: Agenda-setting beneficiary (institutional/identity_locked) — administers the method, collects office, dues, and the movement's lawful-representative position
 *   - trade_union_bureaucracies: Secondary beneficiary (organized/identity_locked) — collects bargaining legitimacy and official tenure, pays in lost militancy
 *   - employer_associations: Incidental beneficiary (powerful/arbitrage) — receives the class-peace dividend; bound by nothing
 *   - liberal_democratic_state: Incidental beneficiary (institutional/arbitrage) — receives order and legitimation; concedes and revokes at will
 *   - industrial_working_class: Dual-positioned constituency (organized/constrained) — receives reforms, pays in deferral and demobilization
 *   - revolutionary_militants: Primary target (moderate/identity_locked) — bears expulsion, surveillance, and periodic police action as 'adventurist'
 *   - insurgent_council_movements: Sharpest target (powerless/trapped) — dispersed or absorbed whenever they outrun the electoral calendar
 *   - colonized_subjects_of_empires: Excluded voice (powerless/trapped) — outside the electorate, inside the costs of the stabilized order
 *   - labor_movement_historians: Analytical observer (analytical/analytical) — sees the full century-long structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.55).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualist Method Discipline (Manifesto Revolutionary-Method Kernel)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political philosophy / revolutionary theory / historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'e0f7a335-ee72-4754-9bb6-54cf150d238d').
narrative_ontology:cs_kernel_codification('e0f7a335-ee72-4754-9bb6-54cf150d238d', fixed_text).
narrative_ontology:cs_authority_grounding('e0f7a335-ee72-4754-9bb6-54cf150d238d', lineage).
narrative_ontology:cs_interpretation_layer_present('e0f7a335-ee72-4754-9bb6-54cf150d238d').
narrative_ontology:cs_reading_relation('e0f7a335-ee72-4754-9bb6-54cf150d238d', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('e0f7a335-ee72-4754-9bb6-54cf150d238d', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('e0f7a335-ee72-4754-9bb6-54cf150d238d', foundational, existing_democratic_structures_sufficient_for_transition).
narrative_ontology:cs_axiom_status(existing_democratic_structures_sufficient_for_transition, holdable).
narrative_ontology:cs_axiom_grounding('e0f7a335-ee72-4754-9bb6-54cf150d238d', existing_democratic_structures_sufficient_for_transition, empirically_contingent).
narrative_ontology:cs_axiom('e0f7a335-ee72-4754-9bb6-54cf150d238d', foundational, majority_consent_precondition_for_legitimate_transformation).
narrative_ontology:cs_axiom_status(majority_consent_precondition_for_legitimate_transformation, holdable).
narrative_ontology:cs_axiom_grounding('e0f7a335-ee72-4754-9bb6-54cf150d238d', majority_consent_precondition_for_legitimate_transformation, deontological).
narrative_ontology:cs_axiom('e0f7a335-ee72-4754-9bb6-54cf150d238d', secondary, reform_accumulation_transforms_property_relations).
narrative_ontology:cs_axiom_status(reform_accumulation_transforms_property_relations, holdable).
narrative_ontology:cs_axiom_grounding('e0f7a335-ee72-4754-9bb6-54cf150d238d', reform_accumulation_transforms_property_relations, instrumental).
narrative_ontology:cs_reference_frame('e0f7a335-ee72-4754-9bb6-54cf150d238d', electoral_majority_gradual_transition).
narrative_ontology:cs_drift_state('e0f7a335-ee72-4754-9bb6-54cf150d238d', contemporary_post_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0f7a335-ee72-4754-9bb6-54cf150d238d', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, insurgent_council_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, employer_associations).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, industrial_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, industrial_working_class).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_road_to_socialism).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, institutional_continuity_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run mass electoral parties that select candidates, write programs, and enforce method discipline: factions advocating extra-parliamentary action are expelled or frozen out, and the party defines what counts as serious working-class politics. They receive votes, dues, and offices proportional to their position as the movement's lawful representative. Leaving the method would mean schism — every attempted break (1917, 1920, the 1980s defections) cost them a wing — so the method and the party hold each other in place.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, identity_locked, national).

% Negotiate wages and conditions inside legally recognized bargaining frameworks the method helped build, and hold paid official positions whose tenure depends on those frameworks continuing. They deliver member gains through contracts rather than stoppages, and when members strike outside agreed windows they bring them back in — absorbing member anger as the price of their seat at the table.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, payer).

% Accept higher wages, taxes, and recognition in exchange for the survival of private property and managerial control, receiving the class-peace dividend: no expropriation, a disciplined labor supply, and stoppages confined to negotiable windows. They fund moderate parties, threaten investment strikes when reforms run deep, and retain the option of backing authoritarian alternatives if the parliamentary channel ever turns genuinely confiscatory — an option the method's continuation keeps dormant.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, employer_associations, beneficiary,
    powerful, generational, arbitrage, national).

% Grants the concessions — suffrage expansion, welfare, bargaining rights — that the method converts into working-class attachment to parliamentary forms, and receives in return the demobilization of insurrectionary threats. It keeps the ultimate coercive apparatus in reserve throughout, conceding when pressure peaks and revoking or repressing when the balance shifts, and is bound by none of the commitments it extends.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, beneficiary,
    institutional, generational, arbitrage, national).

% Vote, pay dues, and staff the parties and unions; receive expanded franchises, welfare provisions, wage floors, and legal strike rights in return. The same channels defer the larger transformation indefinitely and demobilize direct action: when they move faster than the electoral calendar — sit-down strikes, factory occupations — their own leaders and parties pull them back. Opting out of both party and union leaves them individually exposed to employers and the state.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, industrial_working_class, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, industrial_working_class, payer).

% Organize factions, papers, and study circles committed to transformation on a shorter timetable than elections allow. They face expulsion from the mass parties, loss of union positions, surveillance, and — at the sharpest historical moments — police action carried out with the consent of the social democratic leadership they broke from. Their commitment is constitutive: leaving revolutionary politics would dissolve the identity that organized their lives, so they endure marginalization rather than exit.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    moderate, generational, identity_locked, national).

% Form spontaneously in strike waves and military collapse — factory councils, soldiers' councils, joint action committees — and propose running production and local government directly. The parties and unions fold them into bargaining structures, absorb their leaders, or stand aside while the state disperses them, treating their direct-democratic pretensions as premature. They have no existence between crises and no protection once disbanded.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, insurgent_council_movements, payer,
    powerless, immediate, trapped, regional).

% Live under the colonial administrations that several metropolitan socialist parties voted credits for and staffed; the method's timetable counted votes cast in the metropole, and their liberation appears nowhere on its calendar. They are outside the electorate the method addresses and inside the costs of the order it stabilizes; their objection — that the gradualist bargain was financed by imperial extraction — enters the record mainly through anti-colonial movements' later break from the Second International's heirs.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, colonized_subjects_of_empires, excluded,
    powerless, generational, trapped, global).

% Study the method's century-long record across countries: election statistics, strike data, party archives, split proceedings. They document where the method delivered, where it deferred, and where its enforcement turned on the movement's own left, and publish without holding any position inside the arrangement.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, labor_movement_historians, observer,
    analytical, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mass working-class politics: aggregates dispersed workers into an electorally legible majority, gives the movement durable legal organization with continuous presence in parliaments and bargaining tables, and supplies a shared timetable (win elections, legislate reforms, transform ownership stepwise) that lets millions coordinate without the repression and civil-war risk of insurrectionary bids.
% TRANSFER_FUNCTION: Moves working-class political energy — votes, dues, strike capacity — into electoral and bargaining channels controlled by the party and union apparatuses; moves policy concessions (welfare, wages, rights) from the state to workers on a schedule the state and employers can tolerate; and moves revolutionary initiative away from extra-parliamentary action, where it is disciplined, absorbed, or dispersed.
% ABSENT_VOICES: Colonized subjects of the empires several gradualist parties governed and voted war credits for (1914) would object that the timetable served the metropole's workers while imperial extraction financed the bargain; they are outside the electorate the method addresses. The unorganized poor who fell outside union bargains are similarly absent from the tables where 'working-class' interests were defined.
% DISAPPEARANCE_RATIONALE: If the gradualist discipline vanished overnight, the movement's aggregation frame collapses: mass parties lose their programmatic spine, unions lose the bargaining-legitimacy frame that defines their officials' roles, the state loses its counterparty for class peace, and the suppressed wings — council currents, vanguard currents — immediately contend to inherit the constituency. Left politics rearranges around whichever method captures the apparatus and the street first.
% FOUNDING_PROBLEM: How can a propertyless majority dispossess a propertied minority that controls the state's coercive apparatus, without provoking a civil war the majority loses? The gradualist answer: in countries with universal suffrage, organize the class, win the battle of democracy at the ballot box, and accumulate reforms until property relations transform legally.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by: the split record itself (USPD secession 1917, communist formations 1919–21), whose participants attested the founding problem remained unsolved under this method; labor-history scholarship documenting the interwar crises and the post-1979 rollback of delivered gains; and revolutionary-theory literature (council-communist and vanguard critiques) disputing the sufficiency premise while accepting the problem statement. Right-wing sources dispute the goal rather than the genealogy, so no attestation of the problem statement comes from that quarter.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.40 is moderate by construction of the arrangement: the method delivered real goods (franchise, welfare state, bargaining rights) while deferring the transformative goal past every horizon and charging the movement's left wing for the deferral. Suppression 0.55 is structural first (expulsion machinery, state-cooperation protocols, strike-law limits) and partially internalized second (the 'responsibility' norm that makes militants self-censor before the whips move) — the split is flagged in the omegas rather than resolved, since the scalar cannot distinguish mechanisms. Theater 0.45 at interval end: the electoral and bargaining functions remain real, but in the dominant parties the transformative goal has become largely rhetorical — party survival substituted for transformation (Goodhart drift visible in the theater series climbing from 0.15 to 0.52 before the partial post-2015 re-programmatization pulls it back to 0.45). Accessibility_collapse 0.40: alternatives do not collapse — revolutionary exits stay imaginable, which is exactly why enforcement must persist; a constraint whose alternatives truly collapsed would not need a 0.75 suppression spike in 1920. Resistance 0.60: factional wars, the great splits, and recurring wildcat waves are the arrangement's constant companions. The measurement series run on one shared eight-point grid (all three metrics at every point); the trajectories are non-monotonic because the drivers are external crisis cycles — war and revolution (1920 spike), reconstruction delivery (1945 trough), neoliberal rollback (1979–2008 climb), electoral-left revival and counter-mobilization (2008–2025) — and the oscillation tracks those cycles rather than serving as an intermittent-reinforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the party and union seats the arrangement is the coordination success they built and staff: representation, concessions, continuity. From the militant and council seats the same structure operates as enforced deferral with a suppression apparatus attached — their computed classification should sit nearer the snare end than the story-level claim. The working-class seat straddles: it receives the delivered half and pays the deferred half. The state and employer seats experience the arrangement as cheap stability purchased from others' energy. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: social_democratic_parties (agenda_setter, identity_locked) sit near the beneficiary pole — the method subsidizes their office and identity; trade_union_bureaucracies (beneficiary with payer secondary_role) sit low-mid — they collect bargaining legitimacy but pay in surrendered strike capacity; industrial_working_class (dual-role, constrained exit) lands near symmetric — reforms received against deferral paid; revolutionary_militants (payer, identity_locked) sit near the full-target pole — they bear the suppression and cannot leave without dissolving the identity that constitutes them; insurgent_council_movements (payer, trapped, powerless) sit nearest the pole — dispersed with no recourse; liberal_democratic_state and employer_associations (beneficiaries with arbitrage exit) sit low — subsidized by the arrangement yet bound by none of it. On the receipt surface: the extraction's yield lands on multiple seats (employers take the class-peace dividend, the state takes order and legitimation, the parties take office and dues), and gain_flow names employer_associations as the seat the story most directly establishes as receiving the arrangement's principal yield — the survival of property and managerial control — while the commentary records the plural capture rather than asserting a checked universal negative.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope is what prevents mislabeling in both directions. Reading the arrangement as pure coordination (rope) would erase the militant and council victims — the expulsions, the 1919 crushings, the absorbed strike waves are structural, not incidental. Reading it as pure extraction (snare) would erase the delivered half — the franchise, the welfare state, and the avoided civil wars are real outputs, and the arrangement's persistence is not maintained by coercion alone but by genuine mass attachment. The mandatrophy question — has the mandate outlived its function? — is answered 'contested' rather than 'dead': the founding problem (transforming property relations without civil war) recurs whenever inequality spikes, but the dominant parties' abandonment of the transformative goal drives the theater_ratio toward piton territory without crossing it, because the representation-and-bargaining function remains genuinely performed. The status-contested × world-rearranges combination deliberately avoids the zombie-flag mismatch: the arrangement persists because arrangements depend on it, not because a dead mandate is being theatrically maintained — though the 0.45 theater ratio marks it as drifting toward that condition if the transformative goal stays rhetorical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the democratic_gradualism_reading of kernel manifesto_revolutionary_method; which structural elements of this story would change under the sibling readings (vanguard_rupture_reading, council_communist_reading)?',
    'Compile the sibling stories and diff the structural surfaces: victim sets, beneficiary sets, enforcement objects, and ε.',
    'Under council_communist_reading the insurgent_council_movements seat flips from payer to agenda_setter and the state becomes a target; under vanguard_rupture_reading the liberal_democratic_state becomes the primary target and the party apparatus a rival extractor. ε re-bases in both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: sibling readings change victim/beneficiary structure, not this story''s internal facts.').

omega_variable(
    epsilon_counterfactual_weighting,
    'Does ε=0.40 correctly weight the counterfactual cost of the ruptures this method avoided (civil war, dictatorship) against the deferred-emancipation cost it imposed?',
    'Structured comparison of comparable episodes where the method was abandoned (Russia 1917, Spain 1936) versus sustained (Scandinavia, interwar Austria) — outcome distributions under each branch.',
    'Heavy counterfactual weighting lowers ε (restraint was cheap insurance); weighting realized deferral costs raises ε toward the snare range for the militant seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_counterfactual_weighting, conceptual, 'Counterfactual weighting embedded in the moderate ε estimate.').

omega_variable(
    method_rent_vs_coordination_cost,
    'How much of the measured extraction is apparatus rent (careers, office, and funding dependent on the method persisting) versus genuine coordination cost of aggregating millions into a legible political actor?',
    'Compare party and union resource flows under the method against counterfactual organizational forms with similar aggregation tasks; audit career-path dependence of officials.',
    'A high rent share supports tangled_rope-to-snare drift for the payer seats; a low rent share supports rope certification of the coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_rent_vs_coordination_cost, empirical, 'Separating apparatus rent from legitimate coordination overhead.').

omega_variable(
    suppression_ratchet_vs_temporary,
    'Is the post-2015 suppression rise (candidate bans, strike-law tightening, protest policing) a structural ratchet or a cyclical response to the transient electoral-left surge?',
    'Track enforcement indicators past the surge''s decline: if suppression stays elevated after mobilization recedes, ratchet; if it relaxes, cyclical.',
    'A ratchet confirms the rising suppression_requirement trajectory and pushes payer-seat classification toward snare; a cyclical reading keeps the tangled_rope profile stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_ratchet_vs_temporary, empirical, 'Whether the recent enforcement hardening is permanent or episodic.').

omega_variable(
    colonial_cost_exclusion,
    'The ε referent was authored on the metropolitan movement''s operation; how much does extending the referent to the colonized populations governed by gradualist parties (1914 war credits, colonial administration acquiescence) raise effective extraction?',
    'Re-author ε with colonial subjects included in the governed set; compare against the metropolitan-only baseline.',
    'A materially higher ε would shift the story toward snare at the excluded seat and indict the founding-problem corroboration as metropolitan-scoped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_cost_exclusion, conceptual, 'Scope boundary of the ε referent: metropole-only versus empire-wide.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1895, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1895, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1895, 0.15).
narrative_ontology:measurement_basis(mani_tr_t1895, observed).
narrative_ontology:measurement(mani_tr_t1920, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1920, 0.24).
narrative_ontology:measurement_basis(mani_tr_t1920, observed).
narrative_ontology:measurement(mani_tr_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement_basis(mani_tr_t1945, observed).
narrative_ontology:measurement(mani_tr_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1968, 0.27).
narrative_ontology:measurement_basis(mani_tr_t1968, observed).
narrative_ontology:measurement(mani_tr_t1979, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1979, 0.34).
narrative_ontology:measurement_basis(mani_tr_t1979, observed).
narrative_ontology:measurement(mani_tr_t1991, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1991, 0.47).
narrative_ontology:measurement_basis(mani_tr_t1991, observed).
narrative_ontology:measurement(mani_tr_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2008, 0.52).
narrative_ontology:measurement_basis(mani_tr_t2008, observed).
narrative_ontology:measurement(mani_tr_t2025, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(mani_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mani_be_t1895, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1895, 0.2).
narrative_ontology:measurement_basis(mani_be_t1895, observed).
narrative_ontology:measurement(mani_be_t1920, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement_basis(mani_be_t1920, observed).
narrative_ontology:measurement(mani_be_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1945, 0.28).
narrative_ontology:measurement_basis(mani_be_t1945, observed).
narrative_ontology:measurement(mani_be_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1968, 0.33).
narrative_ontology:measurement_basis(mani_be_t1968, observed).
narrative_ontology:measurement(mani_be_t1979, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1979, 0.36).
narrative_ontology:measurement_basis(mani_be_t1979, observed).
narrative_ontology:measurement(mani_be_t1991, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1991, 0.39).
narrative_ontology:measurement_basis(mani_be_t1991, observed).
narrative_ontology:measurement(mani_be_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2008, 0.43).
narrative_ontology:measurement_basis(mani_be_t2008, observed).
narrative_ontology:measurement(mani_be_t2025, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2025, 0.4).
narrative_ontology:measurement_basis(mani_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1895, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1895, 0.35).
narrative_ontology:measurement_basis(mani_su_t1895, observed).
narrative_ontology:measurement(mani_su_t1920, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement_basis(mani_su_t1920, observed).
narrative_ontology:measurement(mani_su_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement_basis(mani_su_t1945, observed).
narrative_ontology:measurement(mani_su_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement_basis(mani_su_t1968, observed).
narrative_ontology:measurement(mani_su_t1979, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1979, 0.58).
narrative_ontology:measurement_basis(mani_su_t1979, observed).
narrative_ontology:measurement(mani_su_t1991, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1991, 0.44).
narrative_ontology:measurement_basis(mani_su_t1991, observed).
narrative_ontology:measurement(mani_su_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2008, 0.41).
narrative_ontology:measurement_basis(mani_su_t2008, observed).
narrative_ontology:measurement(mani_su_t2025, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(mani_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Manifesto's revolutionary method' covers three structurally distinct method-disciplines, decomposed per the ε-invariance principle into three linked stories: this democratic-gradualist reading (moderate ε; beneficiaries are the party-union apparatus; victims are the militant and council wings), the vanguard-rupture reading (the state as target, party dictatorship as transitional instrument), and the council-communist reading (federated workplace assemblies as sovereign organs replacing both state and vanguard party). Each story carries its own ε, beneficiary/victim sets, and enforcement object; this file links both siblings via affects_constraints. Note the seat inversion: the insurgent council movements that are payers under this reading are the sovereign organ under the council-communist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
