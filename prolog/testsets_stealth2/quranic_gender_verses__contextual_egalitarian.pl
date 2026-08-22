% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Contextual-Egalitarian Reading of the Qur'anic Gender Verses (Maqasid-Mediated Application)
 *   domain: religious/legal-hermeneutic/social
 *
 * SUMMARY:
 *   This story instantiates the contextual-egalitarian reading of the
 *   quranic_gender_verses kernel: verses 4:11, 2:282, and 4:34 are treated as
 *   historically situated, progressive provisions for seventh-century Arabia
 *   whose present-day application must run through the Qur'an's overarching
 *   equity objectives (maqasid), yielding substantively equal inheritance,
 *   testimony, and household-standing claims. The constraint examined is that
 *   interpretive regime itself — the standing arrangement under contest,
 *   assessed by this reading's own lights: neither the egalitarian endpoint
 *   it endorses (which would drive epsilon toward zero for every advocacy
 *   reading) nor the literal arrangement its sibling story examines. The
 *   regime solves a real coordination problem: it lets equality-committed
 *   believers remain inside textual authority instead of choosing between
 *   literal obedience and exit. Its costs are equally real: interpretive
 *   competence is scarce and credentialed, so authority rents pool in the
 *   reformist scholarly-NGO sector; patriarchal kin and literal-application
 *   courts are dispossessed of discretionary power they did not surrender
 *   voluntarily; lay believers trade verification for trust; and every
 *   application is fought over, since the plain-sense reading remains live
 *   and resurgent. Constraint-family note: the siblings
 *   (literal_hierarchical, progressive_abrogation) are separate files with
 *   their own epsilon values, victim sets, and enforcement structures,
 *   differing because their hermeneutic premises assign different binding
 *   force to the same verses; edges link the family.
 *
 * KEY AGENTS:
 *   - reformist_jurists: agenda-setting primary beneficiary (institutional/arbitrage) — operates the interpretive machinery and collects the authority rents
 *   - women_claiming_equal_rights: principal intended beneficiary (organized/identity_locked) — converts the reading into inheritance, testimony, and household claims
 *   - rights_based_ngos: secondary beneficiary (organized/mobile) — litigation, documentation, and advocacy infrastructure
 *   - modernizing_family_law_states: enforcing beneficiary (institutional/constrained) — codifies the reading into family law for legitimacy-preserving reform
 *   - patriarchal_kin_authorities: primary target/payer (moderate/identity_locked) — loses discretionary finality over shares, testimony, and guardianship
 *   - traditional_court_jurists: secondary target/payer (institutional/constrained) — displaced from settled-law application
 *   - lay_believers: dual-positioned mass seat (beneficiary/payer; moderate/constrained) — gains access, pays dependence and conflict costs
 *   - non_elite_unrepresented_women: excluded seat (powerless/trapped) — disputes never reach any interpreter or court
 *   - comparative_islamic_studies_scholars: analytical observer (analytical/analytical) — documents the reading's genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.58).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.48).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.58).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Contextual-Egalitarian Reading of the Qur'anic Gender Verses (Maqasid-Mediated Application)").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "religious/legal-hermeneutic/social").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '98d40548-e3da-41e0-b2ef-ee8bfe25c571').
narrative_ontology:cs_kernel_codification('98d40548-e3da-41e0-b2ef-ee8bfe25c571', fixed_text).
narrative_ontology:cs_authority_grounding('98d40548-e3da-41e0-b2ef-ee8bfe25c571', lineage).
narrative_ontology:cs_interpretation_layer_present('98d40548-e3da-41e0-b2ef-ee8bfe25c571').
narrative_ontology:cs_reading_relation('98d40548-e3da-41e0-b2ef-ee8bfe25c571', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('98d40548-e3da-41e0-b2ef-ee8bfe25c571', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('98d40548-e3da-41e0-b2ef-ee8bfe25c571', foundational, gender_rules_historically_situated_not_timeless).
narrative_ontology:cs_axiom_status(gender_rules_historically_situated_not_timeless, holdable).
narrative_ontology:cs_axiom_grounding('98d40548-e3da-41e0-b2ef-ee8bfe25c571', gender_rules_historically_situated_not_timeless, empirically_contingent).
narrative_ontology:cs_axiom('98d40548-e3da-41e0-b2ef-ee8bfe25c571', foundational, maqasid_equity_supremacy_over_specific_rulings).
narrative_ontology:cs_axiom_status(maqasid_equity_supremacy_over_specific_rulings, holdable).
narrative_ontology:cs_axiom_grounding('98d40548-e3da-41e0-b2ef-ee8bfe25c571', maqasid_equity_supremacy_over_specific_rulings, instrumental).
narrative_ontology:cs_reference_frame('98d40548-e3da-41e0-b2ef-ee8bfe25c571', historicized_maqasid_application).
narrative_ontology:cs_drift_state('98d40548-e3da-41e0-b2ef-ee8bfe25c571', contemporary_post_musawah, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('98d40548-e3da-41e0-b2ef-ee8bfe25c571', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_claiming_equal_rights).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_jurists).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, modernizing_family_law_states).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_kin_authorities).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_court_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, lay_believers).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, lay_believers).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, maqasid_hermeneutic_supremacy).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, historical_contextualism_in_tafsir).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, asbab_al_nuzul_contextual_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in classical Arabic, legal theory, and objectives-of-the-law method, they produce the commentaries, fatwas, curricula, and judicial guidance through which application of the verses is settled in seminaries, courts, and media. Standing, appointments, publication markets, and consultancy income track continued demand for expert mediation of the text. Their expertise travels: university chairs, international advisory bodies, and NGO boards all compete for it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_jurists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, reformist_jurists, beneficiary).

% Press claims to equal inheritance shares, undiminished testimony weight, and protection from unilateral household authority, arguing from the verses' historical setting and the equity objectives. Every claim is advanced inside families, congregations, or courtrooms where rivals answer with the verses' plain sense; wins are real but reversible and costly to pursue. Leaving the tradition would dissolve both the community and the claim itself, so the project is staying-and-transforming.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_claiming_equal_rights, beneficiary,
    organized, generational, identity_locked, global).

% Document cases, train advocates, fund litigation and comparative research, and run the conferences and shadow-report pipelines connecting the reading to concrete legal demands. Programs and budgets renew on donor and grant cycles keyed to the reading's continued traction in courts and policy debates.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, biographical, mobile, global).

% Recodify family law — marriage ages, divorce procedure, custody defaults — while presenting each reform as faithful to revelation rather than concession to outside pressure, drawing on the reading's scholarly output for legitimacy. Enforcement runs through state courts and official councils; retreating is difficult because both reform constituencies and religious establishments monitor compliance.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, modernizing_family_law_states, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, modernizing_family_law_states, agenda_setter).

% Elders and male relatives who divide inheritances, witness contracts, arrange marriages, and direct households according to the differentiated shares and guardian prerogatives. Under the reading, allocations once settled as simple obedience to the text now require answering equity arguments they did not authorize, in front of courts and daughters willing to litigate. Household standing rests on the guardian role itself.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_kin_authorities, payer,
    moderate, biographical, identity_locked, regional).

% Judges and jurists in systems where the differentiated application stood as settled law for generations. The reading forces a fork: adopt the new method, with retraining and career risk before conservative hierarchies, or defend the old application against mounting appellate scrutiny, litigation, and international reporting.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_court_jurists, payer,
    institutional, biographical, constrained, national).

% Congregate, marry, and raise children inside communities where the reading supplies the official account of the verses. They receive access to egalitarian outcomes without leaving the faith, but cannot personally verify the interpretations: the method presupposes classical Arabic, legal theory, and historical training most lack, so trust substitutes for checking. They also absorb the resulting conflict — dueling sermons, relatives divided over shares, standing communal debate about legitimacy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, lay_believers, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, lay_believers, payer).

% Live where inheritance and marriage disputes are settled entirely within kinship custom justified by the verses' plain sense; no trained jurist, court officer, or advocate is reachable. In the reading's institutional world they surface only as anonymized case material in reports and studies.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, non_elite_unrepresented_women, excluded,
    powerless, immediate, trapped, local).

% Reconstruct how the verses have been read across fourteen centuries and trace how the historical-contextual approach emerged from nineteenth- and twentieth-century reform debates. They publish the source materials every side argues from and hold no seat in any enforcing body.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, comparative_islamic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, reformist_jurists).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one coherent method for applying the revealed text inside modern legal and family life: historical situating of the gender verses plus governing equity objectives lets judges, families, and schools act without choosing between textual fidelity and egalitarian conscience, and without splitting the community into obedient-patriarchal and secular-exiting wings.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative discretion away from male kin structures and literal-application courts toward credentialed reformist scholars, NGO advocates, and reformed state codes; moves material claims — inheritance fractions, testimony weight, marital decision authority — from male relatives toward women claimants; moves attention, funding, and careers toward the reformist scholarly-NGO sector.
% ABSENT_VOICES: Women whose inheritance and marriage disputes are settled entirely inside kinship custom and never reach a trained jurist, court, or advocate; and laypeople without classical Arabic or legal-theory training whose access to the reading runs entirely through mediated authority. Neither group sits in the fiqh academies, NGO boards, or reform commissions where the reading's applications are settled.
% DISAPPEARANCE_RATIONALE: Without the contextual-egalitarian settlement, application of the verses reverts toward the plain-sense differentiation: unequal inheritance shares resume wherever custom permits, testimony discounts return in documentary practice, and guardianship prerogatives reassert in marriage and household decisions. Reformist institutions, NGO programs, and state reform projects built on the reading lose their warrant, and a portion of equality-minded believers faces the exact choice the reading existed to remove — literal obedience or exit.
% FOUNDING_PROBLEM: How a community that takes the revealed text as permanently authoritative can reject the gender-differentiated legal outcomes the text's plain sense appears to fix — unequal shares, discounted testimony, male guardianship — without either abandoning the text or abandoning equality.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: conservative juristic bodies that reject this reading nonetheless concede the modern egalitarian pressure it answers, framing their own defenses as responses to that challenge; academic historians of the nineteenth- and twentieth-century Islamic legal-reform debates (the Abduh-through-Rahman arc) document the text-versus-equality collision independently of any advocacy program; state family-law codification records treat the reconciliation as an open administrative problem requiring repeated legislative sessions. The reformist sector is not the sole attestor.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 is the interval-end reading of a deliberately moderate profile: the coordination is real, but authority rents pool in the credentialed sector, displaced parties pay involuntarily, and every application is contested. Suppression 0.48 is a raw structural measure — enforcement exists (curricular control, court gatekeeping, marginalization of literalist instruction within reformist-held institutions) but stops short of reaching private belief; per the framework, only extractiveness is scaled by directionality and scope. Theater 0.38: the interpretive function is substantially real — rulings and codes change — while a growing declaratory layer (communiques, indices, conference resolutions) outruns implementation. Accessibility_collapse 0.45: the plain-sense reading remains fully available and visibly practiced, so alternatives only partially close for anyone inside the dispute. Resistance 0.65: counter-fatwa campaigns, conservative court hierarchies, and household-level refusal meet the reading continuously. All three tracked series share one seven-point grid (1930-2025) with every metric authored at every point, avoiding row-substitution artifacts. Trajectories: base_extractiveness climbs with institutionalization (state adoption, growth of the funded NGO sector) — rent accumulation layered onto coordination — peaking around 2015 and easing slightly as intensifying contestation raises the price of maintaining the reading; theater rises on the same path with the same late easing; suppression_requirement traces enforcement-capacity buildup (print-era persuasion at 0.20 through state-council enforcement near 0.50) then erosion after 2015 as counter-mobilization strains reformist institutional control. Identity locks bind both poles: women claimants cannot exit without dissolving the claim itself; kin authorities cannot exit the guardian role without dissolving their standing — the same fusion mechanism stabilizes the regime's coalition and its opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting jurist seat the regime is a completed liberation: method delivered, claims enabled, institutions built. From the payer seats it is dispossession: authority exercised for generations over shares and households removed by arguments those seats never accepted, enforced through courts and donor-funded pressure. From the women's seat the picture splits again — the reading is the only vehicle for the claims, yet its costs (family conflict, delay, reversal risk) land disproportionately on claimants. From the pews it is a trust relation: outcomes promised, verification impossible. The engine computes these divergent per-seat types from the same structural record; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations plus exit atoms reproduce the true structural relations, so no directionality overrides are needed. The four beneficiaries derive low d near the subsidized end; the jurists lowest of all, since arbitrage-grade exit and agenda-setting place them nearest the pure-beneficiary pole despite their dual collection-and-administration position. The two payers derive high d: patriarchal kin authorities near-full-target because the identity lock leaves no exit from the role being displaced; traditional court jurists slightly lower, since constrained-but-existing career mobility softens the trap. Lay believers, declared beneficiary with payer costs, derive near-symmetric d: access gained roughly balances dependence absorbed. Larger spatial scopes on the global seats modestly amplify effective extraction through harder verification; the local excluded seat's trapping is registered through exit, not scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — holding textual authority and egalitarian commitment together without schism — remains live, so nothing here resolves into mandatrophy: the mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges, no zombie flag, and the arrangement plainly depends-on. Classification discipline cuts both ways. Reading this as a snare would erase the genuine collision-resolving coordination that keeps equality-minded believers inside the tradition; reading it as a rope would erase the authority rents pooling in the credentialed sector and the involuntary dispossession of kin and court authorities — hence the tangled-rope claim with both sides of the ledger explicitly declared (beneficiaries, victims, active enforcement all present). Watch item: theater_ratio trends upward across the series as declaratory activity outpaces implemented rulings; if implementation stagnates while the interpretive apparatus keeps performing, the structure drifts toward piton dynamics — administration persisting after delivery — and the mandate question reopens for a successor story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_classification_delta,
    'This story authors one reading (contextual_egalitarian) of the kernel quranic_gender_verses; how would classification shift under the sibling readings literal_hierarchical and progressive_abrogation, which assign different hermeneutic statuses to the same verses?',
    'Generate the sibling readings as separate constraint stories — separate epsilon, beneficiaries, victims, enforcement — per the epsilon-invariance principle, and compare computed classifications across the family; never average across readings within one file.',
    'Under literal_hierarchical the victim set centers on women bearing the differentiated shares (high extraction, snare-flavored); under progressive_abrogation extraction concentrates in whichever authority certifies supersession; the moderate profile authored here holds only for the contextual_egalitarian instantiation and must not be read as a verdict on the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_classification_delta, conceptual, 'Committer-frame uncertainty: this file is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    interpretive_rent_necessity,
    'Are the authority rents accruing to credentialed reformist jurists a necessary cost of any scripture-governed order (all mediated revelation requires trained interpreters), or excess extraction reducible by mass textual literacy?',
    'Compare communities with widespread vernacular-tafsir access and Quranic-literacy programs against mediation-dependent communities: does measured dependence on credentialed intermediaries and rent capture fall without loss of interpretive coherence?',
    'If rents fall with literacy, the constraint drifts toward rope (pure coordination with minimal overhead); if they persist, the mediation structure is load-bearing extraction and the classification deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_rent_necessity, empirical, 'Whether the credentialed-intermediary layer is coordination cost or capture.').

omega_variable(
    women_victim_exit_durability,
    'Does the reading durably move women out of the victim set, or does it relocate their costs into litigation burdens, family conflict, and reversal risk that women disproportionately absorb?',
    'Longitudinal outcome data on equal-share and testimony claims: grant rates, time-to-resolution, costs borne by claimants, and rates of reversal after initial wins, disaggregated by jurisdiction and court orientation.',
    'If costs merely relocate, effective extraction on the women''s seat is higher than the beneficiary declaration suggests, the derived directionality for that seat rises, and the overall classification deepens despite the structural delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_victim_exit_durability, empirical, 'Durability of the women''s exit from the victim set under this reading.').

omega_variable(
    suppression_vs_boundary_keeping,
    'Is the enforcement effort maintaining this reading against literal resurgence better described as suppression of a rival reading that serves much of the community, or as ordinary boundary-keeping for a coherent method?',
    'Track what enforcement actually targets: exclusion of literalist instruction from institutions the reformist coalition controls, versus penalties reaching private belief, worship, and family practice.',
    'If enforcement reaches private practice, the authored suppression understates the structural fact and the snare-side component grows; if confined to institutional gatekeeping, current values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_boundary_keeping, conceptual, 'Whether maintenance activity crosses from method-defense into rival-suppression.').

omega_variable(
    cs_framing_underdetermination,
    'The declared CS framing (fixed_text kernel, lineage authority, functioning maqasid interpretation layer) reflects the reformist self-description; an alternative framing reads the authority structure as grounded in extraction — the credentialed class benefits from preventing kernel revision — which would change the commitment-system classification.',
    'Test whether interpretive authority tracks demonstrated competence and transmitted license (lineage behavior) or defense of the mediation monopoly (extraction behavior): examine appointment criteria, licensing chains, and the treatment of autodidact interpreters who reach egalitarian conclusions by other routes.',
    'Under the extraction framing, authority_grounding shifts accordingly, the interpretation layer reads as a rent buffer rather than a drift absorber, and coupling diagnostics strengthen toward capture findings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same authority structure produce different CS patterns; signals guiding the lineage choice are the documented ijaza/transmission credentials of the reading''s leading figures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 1930, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1930, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(qura_tr_t1950, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(qura_tr_t1970, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1970, 0.26).
narrative_ontology:measurement(qura_tr_t1990, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1990, 0.31).
narrative_ontology:measurement(qura_tr_t2005, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(qura_tr_t2015, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(qura_tr_t2025, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(qura_be_t1930, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1930, 0.33).
narrative_ontology:measurement(qura_be_t1950, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1950, 0.39).
narrative_ontology:measurement(qura_be_t1970, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1970, 0.46).
narrative_ontology:measurement(qura_be_t1990, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(qura_be_t2005, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(qura_be_t2015, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(qura_be_t2025, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1930, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1930, 0.2).
narrative_ontology:measurement(qura_su_t1950, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(qura_su_t1970, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(qura_su_t1990, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1990, 0.46).
narrative_ontology:measurement(qura_su_t2005, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(qura_su_t2015, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(qura_su_t2025, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% 'What the Qur'an says about women' is a colloquial label covering three structurally distinct constraints, decomposed per the epsilon-invariance principle: this contextual-egalitarian instantiation (moderate epsilon; rents pool in credentialed mediators; women exit the victim set), the literal-hierarchical sibling (high epsilon; women are the extracted-from set; enforcement by traditional authority), and the progressive-abrogation sibling (epsilon concentrated in whatever body certifies supersession). Upstream-downstream: the literal reading is the historical baseline both egalitarian readings define themselves against; the abrogation reading and this reading exert reciprocal legitimacy pressure on each other's methods. Edges are declared reciprocally across the family files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
