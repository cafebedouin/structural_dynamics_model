% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine-Covenant Title to the Land (Religious Obligation Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   jewish_self_determination: the claim that the Jewish title to the land
 *   derives from divine covenant, making territorial sovereignty a religious
 *   obligation that stands independent of, and prior to, secular political
 *   frameworks. As a standing arrangement this reading operates through a
 *   religious-state entanglement: a rabbinic establishment that authors the
 *   doctrine, a settlement enterprise that receives the arrangement's
 *   material outputs, a national-religious public whose identity presupposes
 *   the frame, and a foreclosure of negotiated-border politics that binds
 *   non-believers (Palestinians under the associated administration, secular
 *   Israeli negotiators, international legal bodies) to consequences of a
 *   premise they never accepted. The constraint presents itself as natural
 *   law (divine command, immutable as physics for the believer) but operates
 *   as a constructed, actively enforced arrangement with identifiable
 *   beneficiaries and identifiable payers. KEY AGENTS (by structural
 *   relationship): - rabbinic_establishment: Agenda setter
 *   (institutional/identity_locked) — authors and administers the doctrinal
 *   kernel - settlement_enterprise: Primary beneficiary with administrative
 *   reach (organized/identity_locked) — receives land, budget, legal shield -
 *   religious_zionist_public: Beneficiary constituency
 *   (organized/identity_locked) — collects meaning and cohesion -
 *   diaspora_jewish_institutions: Secondary beneficiary (powerful/mobile) —
 *   collects identity legitimation at distance -
 *   palestinian_residents_of_occupied_territories: Primary target
 *   (powerless/trapped) — bears the arrangement's physical costs -
 *   secular_two_state_advocates: Target of foreclosure (moderate/constrained)
 *   — their instrument is ruled out as category error -
 *   haredi_anti_zionist_communities: Excluded voice
 *   (organized/identity_locked) — internal-tradition objection kept outside
 *   the conversation - international_mediation_bodies: Analytical observer
 *   (institutional/analytical) — adjudicates under a framework the reading
 *   deems irrelevant
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.72).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.68).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine-Covenant Title to the Land (Religious Obligation Reading)").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '733120ba-1d60-446f-9c50-9b735033efda').
narrative_ontology:cs_kernel_codification('733120ba-1d60-446f-9c50-9b735033efda', fixed_text).
narrative_ontology:cs_authority_grounding('733120ba-1d60-446f-9c50-9b735033efda', lineage).
narrative_ontology:cs_interpretation_layer_present('733120ba-1d60-446f-9c50-9b735033efda').
narrative_ontology:cs_reading_relation('733120ba-1d60-446f-9c50-9b735033efda', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('733120ba-1d60-446f-9c50-9b735033efda', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('733120ba-1d60-446f-9c50-9b735033efda', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('733120ba-1d60-446f-9c50-9b735033efda', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('733120ba-1d60-446f-9c50-9b735033efda', foundational, land_grant_unconditional_divine_bequest).
narrative_ontology:cs_axiom_status(land_grant_unconditional_divine_bequest, holdable).
narrative_ontology:cs_axiom_grounding('733120ba-1d60-446f-9c50-9b735033efda', land_grant_unconditional_divine_bequest, theological).
narrative_ontology:cs_axiom('733120ba-1d60-446f-9c50-9b735033efda', foundational, sovereignty_religious_duty_not_secular_choice).
narrative_ontology:cs_axiom_status(sovereignty_religious_duty_not_secular_choice, holdable).
narrative_ontology:cs_axiom_grounding('733120ba-1d60-446f-9c50-9b735033efda', sovereignty_religious_duty_not_secular_choice, theological).
narrative_ontology:cs_reference_frame('733120ba-1d60-446f-9c50-9b735033efda', covenantal_land_grant_binding_all_generations).
narrative_ontology:cs_drift_state('733120ba-1d60-446f-9c50-9b735033efda', post_disengagement_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('733120ba-1d60-446f-9c50-9b735033efda', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_public).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, rabbinic_establishment).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, diaspora_jewish_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_occupied_territories).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_two_state_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chief rabbinate, yeshiva heads, and halakhic decisors who author the doctrinal positions identifying the land's grant as unconditional and sovereignty over it as obligatory. They certify which political arrangements accord with the tradition, train the cadre that staffs settlement institutions, and discipline deviation through communal standing. Their office's authority rests on the covenant frame remaining operative; stepping outside it would dissolve the basis of their own standing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, rabbinic_establishment, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Municipal councils, yeshivot, and advocacy organizations administering communities beyond the 1967 lines. They receive housing subsidies, infrastructure budgets, security deployment, and legal defense from the state, and they organize the political pressure that keeps territorial withdrawal off the agenda. Members' homes, schools, and livelihoods sit physically inside the arrangement's output; relocation would mean abandoning built lives, not paying a fee.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, settlement_enterprise, agenda_setter).

% The national-religious public, for whom the covenant reading supplies the answer to why Jews should hold state power at all. It organizes their military service, residence choices, voting, and child-rearing into a single meaningful project and binds them to a community whose membership presupposes the frame. Leaving the frame would cost them community, meaning, and family continuity rather than money.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_public, beneficiary,
    organized, generational, identity_locked, national).

% Federations, synagogues, and advocacy organizations outside Israel that draw on the covenant narrative for identity cohesion, fundraising energy, and an account of why collective Jewish life matters. They channel donations and lobbying support toward the arrangement and absorb almost none of its on-the-ground cost.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diaspora_jewish_institutions, beneficiary,
    powerful, generational, mobile, global).

% Several million people living under the military and civil administration that the settlement presence requires. They experience land appropriation, movement restrictions, home demolitions, and a parallel legal order subordinated to the settlement project. They hold no citizenship in the state that governs them and no sovereign of their own able to negotiate on equal terms; leaving means permanent displacement.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_occupied_territories, payer,
    powerless, biographical, trapped, regional).

% Israeli politicians, diplomats, jurists, and voters who seek a negotiated border and hold that sovereignty's terms must be settled by agreement rather than theological title. Each electoral cycle their position loses ground to parties running on the covenant frame; their preferred instrument, negotiation, is exactly what the reading classifies as a category error. Their exit is emigration or frame-switch, both personally costly.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_two_state_advocates, payer,
    moderate, biographical, constrained, national).

% Ultra-Orthodox circles (Satmar, Neturei Karta, and related groups) who affirm the covenant's sanctity yet hold that human sovereignty before the messianic age violates it. They would challenge the political-obligation reading from inside the textual tradition itself, but they stand outside Israeli coalition politics and outside the Zionist conversation their objection targets.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, haredi_anti_zionist_communities, excluded,
    organized, generational, identity_locked, global).

% UN organs, the International Court of Justice, and third-party mediators who adjudicate the territorial question under international law. They treat sovereignty's terms as matters of treaty, resolution, and occupied-territory convention, take testimony from the other seats, and issue rulings that the covenant frame treats as irrelevant to a title that precedes and outranks them.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_mediation_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the national-religious community around a shared sacred purpose: it explains exile and return as a single arc, organizes settlement, mobilizes service and sacrifice, and folds individual life decisions (where to live, how to serve, how to vote) into one intergenerational project. It also solves an identity-coordination problem for diaspora institutions by supplying a master narrative for collective continuity.
% TRANSFER_FUNCTION: Moves land, state budget, security deployment, legal protection, and agenda-setting power toward the settlement enterprise and religious institutions; moves decision-making authority over territory away from negotiated and electoral channels and into religiously defined obligation; moves the costs of the arrangement (appropriation, restriction, foreclosed compromise) onto Palestinians in the territories and onto advocates of negotiated settlement.
% ABSENT_VOICES: Haredi anti-Zionist communities would object from inside the textual tradition that the political-obligation reading mistakes a suspended promise for a present duty; they are organizationally excluded from the coalition that runs the arrangement. Palestinian residents of the territories bear its consequences without any seat in the Israeli coalition that sets it. Secular diaspora Jews who read the covenant as liturgy rather than land deed are spoken for by institutions they do not control.
% DISAPPEARANCE_RATIONALE: If the covenant-as-political-obligation frame vanished overnight, the settlement enterprise would lose its sacral warrant and its most reliable electoral shield, coalition arithmetic would shift toward secular bargaining, the foreclosure of negotiated borders would lift, and the conflict's religious dimension (holy-site politics, messianic activism, settlement as devotion) would reorganize around purely strategic claims. Believing communities would rebuild their meaning-structures; the region's physical geography would change only through the politics that follows.
% FOUNDING_PROBLEM: Two layered problems. The ancient one: sustaining collective identity and hope across two millennia of statelessness, for which the covenant served as memory and promise. The modern one, acute after 1967: giving Jewish state power theological legitimacy, answering why a people whose tradition matured under diaspora restraint should exercise sovereignty at all, and what to do with territory the tradition describes as promised.
% FOUNDING_PROBLEM_CORROBORATION: Academic Jewish studies and history-of-religion scholarship, sitting outside the beneficiary set, corroborates the covenant's antiquity and its identity-sustaining function across the diaspora centuries. No source outside the beneficiary set corroborates the specifically political-obligation reading: haredi anti-Zionist communities attest the covenant while denying that it mandates human sovereignty, and Palestinian and international legal scholarship attests the arrangement's costs rather than its warrant. That corroborative absence is itself signal.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the arrangement transfers land, budget, and agenda-control to identifiable seats while imposing appropriation, restriction, and foreclosed compromise on non-consenting populations; the referent is the standing arrangement as it operates over everyone it governs, not the believing community's internal experience of it (see omega framework_relative_epsilon). Suppression (0.68) is authored as a raw structural property, unscaled by power or scope: it combines heresy-adjacent communal discipline inside the frame, state coercion in the territories, and systematic delegitimization of compromise politics. Theater ratio is moderate-low (0.25): the devotional and communal practice is largely sincere and functional for participants; the performative share (piety displayed as policy justification, ritualized invocations shielding material interests) is real but a minority of activity. Accessibility collapse is moderate (0.45): once the frame is understood, alternatives (secular nationalism, negotiated compromise, diasporism) remain socially alive and are actively pursued, unlike a natural law whose alternatives vanish on comprehension. Resistance is substantial (0.62): court challenges, mass protest movements, Palestinian resistance, international rulings, and intra-Jewish rejection all press against the arrangement and must be continuously absorbed. The measurement series run on one shared time grid (T=0..50, six points, all three metrics at every point) so no metric's end-state value is silently substituted into earlier rows; all points are observed historical record spanning roughly 1967 to the present, during which settlement depth, enforcement machinery, and foreclosure hardened monotonically.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structure. From the believer seats (rabbinic establishment, settlement core, national-religious public), the arrangement is obedience to a command older than the state: personal cost reads as devotion, extraction reads as fulfillment, and the type computes toward mountain-or-rope territory because nothing is taken that was not gladly given. From the payer seats (Palestinian residents, secular negotiators), the same structure is an imposed order binding them to a premise they never entered: the type computes toward snare-flavored extraction with no coordination benefit reaching them at all. The diaspora beneficiary seat computes a cheap, distant subsidy of meaning. The engine computes this divergence from the structural data; the authored claim does not adjudicate it, and the wide gap is the story's central measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the rabbinic establishment (collects authority), the settlement enterprise (collects land, budget, legal shield), the national-religious public (collects meaning), and diaspora institutions (collects legitimation at negligible cost, arbitrage-grade exit). Victim declarations drive high directionality for Palestinian residents (trapped, full-target end) and secular two-state advocates (constrained, near-target). Identity-lock amplifies target-side extraction nowhere here but amplifies beneficiary-side stickiness: the believing seats cannot cheaply leave the frame that subsidizes them, which stabilizes the arrangement against defection from within. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is layered and its status contested: sustaining identity through exile (ancient, corroborated externally as a real historical function) versus legitimating state power theologically (modern, corroborated by no one outside the beneficiary set). The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement is load-bearing for its beneficiaries regardless of whether its warrant holds, which is the capture signature. Mandatrophy resolution here prevents mislabeling in both directions: reading the arrangement as pure snare erases the genuine identity-coordination function that voluntarily binds millions and mispredicts the durability of their assent; reading it as pure rope or mountain erases the foreclosure imposed on non-believers and mispredicts the conflict's irreducibility. The tangled_rope claim holds both halves: real coordination, asymmetric payment, active enforcement. If the identity frame broke (mass secularization of the national-religious public, or a rabbinic reversal on political obligation), the coordination half would decay faster than the material half, and the residue would present as piton-like maintenance of privileges whose warrant had lapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (religious_covenant_reading) of the contested kernel jewish_self_determination; which structural elements do the four sibling readings relocate, and where exactly is the disagreement located?',
    'Comparative compilation of the four sibling stories (liberal_nationalist, indigenous_return, settler_colonial, diasporist). The disagreement locates on the source-of-claim axis: divine grant versus equal-national-right versus indigenous continuity versus colonial mechanism versus anti-territorial pluralism. Source-of-claim determines the victim set and therefore epsilon.',
    'Under the liberal-nationalist or indigenous-return reading the victim set shrinks (nothing forecloses secular negotiation as such) and epsilon falls toward rope range; under the settler-colonial reading the victim set expands to all Palestinians displaced from 1948 forward and epsilon rises toward snare range; under the diasporist reading the arrangement itself becomes the harm and the victim set includes Jewish citizens of the state. This file instantiates only the covenant reading''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a five-way contested kernel; sibling readings are separate constraints with different victim sets and epsilon.').

omega_variable(
    framework_relative_epsilon,
    'Is epsilon properly assessed inside the believing framework, where obeying a perceived divine command extracts nothing because compliance is the point, or across the full population the arrangement governs, including non-believers bound by its consequences?',
    'Per-seat computation already carries both answers: believer seats compute near-zero personal loss while payer seats compute heavy imposed cost. Corpus-level comparison of seat classifications establishes which referent the indexical system tracks for kernel readings whose framework is itself contested.',
    'Within-framework epsilon sits near 0.1 (costly obedience voluntarily assumed, rope-like); population-wide epsilon sits near 0.72. The authored scalar takes the population-wide referent because the arrangement binds non-believers through state power, and the reading''s claim to internal immunity cannot extend over people who never entered the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_relative_epsilon, conceptual, 'Whether the constraint''s extractiveness is measured inside the believing frame or across all governed populations.').

omega_variable(
    divine_command_epistemic_status,
    'Is the covenant a genuinely binding divine command, which would lend the constraint mountain-like immutability for any theistic frame, or a constructed identity narrative maintained by institutions with interests in its maintenance?',
    'Not resolvable by evidence in either direction; resolution consists in adopting or refusing the theistic premise, which is a framing choice rather than a measurement. The corpus tracks it as an open framing variable rather than pretending neutrality.',
    'If the theistic premise is granted, the immutability claim strengthens and the constraint approaches mountain character for consenting believers (while remaining imposed construction for everyone else); if refused, the constraint is fully constructed and the tangled_rope reading stands with no residual natural-law character. The presentation-as-natural-law is thus seat-relative, which is exactly what the per-seat computation registers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_command_epistemic_status, conceptual, 'Mountain-presentation versus constructed-normative-status of the covenant claim; unresolvable without adopting or refusing the theistic premise.').

omega_variable(
    volition_enforcement_composition,
    'How much of the arrangement''s persistence is voluntary belief versus state subsidy, legal privilege, and coercive enforcement?',
    'Counterfactual fiscal analysis: model settlement-community viability absent state transfers, security deployment, and legal defense; survey religious commitment independent of institutional incentive structures.',
    'A high voluntary share supports a rope-leaning classification (coordination sustained by assent, extraction limited to what assent tolerates); a high coercive share supports a snare-leaning classification (extraction sustained by machinery that outlives or overrides assent). The current mixed profile is what makes the tangled_rope claim the honest middle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volition_enforcement_composition, empirical, 'Composition of persistence between voluntary belief and enforced structure.').

omega_variable(
    messianic_absolutism_trajectory,
    'Is the reading''s political form hardening toward maximalist irredentism (annexation legislation, Temple Mount activism, rejection of any withdrawal) or stabilizing under institutional moderation?',
    'Track annexation bills, settler-population growth rates, rabbinic rulings on territorial compromise, and enforcement incidents over the coming decade.',
    'Hardening drives suppression and extractiveness further up and pushes the arrangement toward snare; stabilization holds the tangled_rope classification. The rising suppression series in this story assumes continued hardening; a reversal would flatten it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_absolutism_trajectory, empirical, 'Trajectory of the reading''s political form between absolutist and moderated poles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__religious_covenant_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__religious_covenant_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__religious_covenant_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__religious_covenant_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__religious_covenant_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__religious_covenant_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__religious_covenant_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__religious_covenant_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__religious_covenant_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__religious_covenant_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__religious_covenant_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__religious_covenant_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Jewish claim to the land.' The single natural-language concept covers five structurally distinct claims with different epsilon values, victim sets, and failure modes; per the epsilon-invariance principle they are authored as separate stories linked by network edges rather than forced into one observable-dependent story. This file is the religious_covenant_reading member. Upstream/downstream: the covenant reading structurally reinforces the indigenous_return_reading (shared beneficiaries deploy both) and sharpens the settler_colonial_reading (its entrenchment supplies that critique's central evidence), while coexisting with the liberal_nationalist and diasporist readings as opposed poles of a live multi-party dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
