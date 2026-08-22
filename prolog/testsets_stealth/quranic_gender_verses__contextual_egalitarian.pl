% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Contextual-Egalitarian Reading of the Qur'anic Gender Verses (Maqasid Filter)
 *   domain: religious/legal/hermeneutic/gender
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the legal force of Q.4:11 (inheritance shares), Q.2:282 (testimony
 *   weight), and Q.4:34 (guardianship and discipline). The
 *   contextual_egalitarian reading holds these verses to be historically
 *   situated, deliberately progressive interventions within seventh-century
 *   Arabian practice — real improvements in their own setting — whose
 *   continued force runs through purposive (maqasid) reinterpretation rather
 *   than through the transmitted letter. The epsilon referent is the standing
 *   arrangement under contest: the actual legal operation of these verses in
 *   personal-status law, assessed by this reading's own lights. That
 *   assessment yields moderate extraction — the reading does not regard the
 *   original rules as predatory (they were, on its account, advances), but it
 *   regards their continued unfiltered application as imposing unjustified
 *   material burdens on women, and it recognizes that its own remedy routes
 *   women's claims through credentialed mediation. Claimed type and metrics
 *   are authored independently: the claim is tangled_rope because the reading
 *   genuinely coordinates (it resolves the scripture-versus-equality
 *   collective action problem without schism) while asymmetrically extracting
 *   (interpretive precedence concentrates in the reformist scholar class;
 *   displaced elites and courts pay; women's gains remain
 *   certification-dependent). The engine computes per-seat classifications
 *   from the structural data; where a computed seat diverges from this claim,
 *   that divergence is data, not error. KEY AGENTS (by structural
 *   relationship): - reformist_jurists: Agenda-setting interpreter class
 *   ([organized]/[identity_locked]) — administers maqasid certification,
 *   collects interpretive precedence - women_rights_claimants: Material
 *   stakeholder ([powerless]/[identity_locked]) — gains enforceable claims
 *   where adopted, bears backlash and mediation-dependence costs -
 *   patriarchal_religious_elites: Displaced authority
 *   ([institutional]/[constrained]) — loses discretionary power, retains
 *   co-optation leverage - traditional_sharia_courts: Adjudicative payer
 *   ([institutional]/[constrained]) — docket discretion narrows under
 *   equity-grounded review - rights_based_ngos: Advocacy beneficiary
 *   ([organized]/[mobile]) — converts the reform gap into mandate and funding
 *   - lay_muslim_communities: Communal bearer ([moderate]/[constrained]) —
 *   carries the legitimacy-conflict friction - secular_feminist_critics:
 *   Excluded critic ([organized]/[mobile]) — objects from outside the
 *   admissible conversation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.52).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.56).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.52).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Contextual-Egalitarian Reading of the Qur'anic Gender Verses (Maqasid Filter)").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "religious/legal/hermeneutic/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, 'f39adc95-73b3-469b-be7e-d764ebf9fc20').
narrative_ontology:cs_kernel_codification('f39adc95-73b3-469b-be7e-d764ebf9fc20', fixed_text).
narrative_ontology:cs_authority_grounding('f39adc95-73b3-469b-be7e-d764ebf9fc20', lineage).
narrative_ontology:cs_interpretation_layer_present('f39adc95-73b3-469b-be7e-d764ebf9fc20').
narrative_ontology:cs_reading_relation('f39adc95-73b3-469b-be7e-d764ebf9fc20', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('f39adc95-73b3-469b-be7e-d764ebf9fc20', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('f39adc95-73b3-469b-be7e-d764ebf9fc20', foundational, verses_historically_situational_not_timeless).
narrative_ontology:cs_axiom_status(verses_historically_situational_not_timeless, holdable).
narrative_ontology:cs_axiom_grounding('f39adc95-73b3-469b-be7e-d764ebf9fc20', verses_historically_situational_not_timeless, empirically_contingent).
narrative_ontology:cs_axiom('f39adc95-73b3-469b-be7e-d764ebf9fc20', foundational, maqasid_equity_overrides_literal_application).
narrative_ontology:cs_axiom_status(maqasid_equity_overrides_literal_application, holdable).
narrative_ontology:cs_axiom_grounding('f39adc95-73b3-469b-be7e-d764ebf9fc20', maqasid_equity_overrides_literal_application, theological).
narrative_ontology:cs_reference_frame('f39adc95-73b3-469b-be7e-d764ebf9fc20', maqasid_filtered_progressive_revelation).
narrative_ontology:cs_drift_state('f39adc95-73b3-469b-be7e-d764ebf9fc20', contemporary_personal_status_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f39adc95-73b3-469b-be7e-d764ebf9fc20', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_jurists).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_rights_claimants).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, lay_muslim_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_religious_elites).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_sharia_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, women_rights_claimants).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, lay_muslim_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in usul al-fiqh and maqasid methodology, they publish reinterpretations of the inheritance, testimony, and guardianship verses, certify which readings count as legitimate, staff state reform commissions, and anchor the conference and fatwa circuits through which contextualist rulings travel. Their standing exists only inside the tradition: stepping outside it as secular critics would dissolve the authority their credentials carry. Interpretive precedence flows to them wherever courts and legislatures adopt contextualist reasoning; they also absorb the polemical costs of traditionalist attack.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_jurists, agenda_setter,
    organized, generational, identity_locked, global).

% Campaign for equal inheritance and testimony provisions, fund litigation and drafting support for family-law reform, translate maqasid arguments into treaty-report language, and raise recurring support on the gap between formal commitments and courtroom practice. They can shift portfolios to other rights files if this one closes.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, biographical, mobile, global).

% Hold the material stakes: inheritance shares, witness-weight rules, and obedience and discipline provisions shape household economics and legal standing. Where contextualist rulings are adopted they gain enforceable claims; reaching those rulings typically requires a recognized scholar's certification, and asserting them draws social sanction from kin and congregations. Leaving the community to escape the dispute is unthinkable for most, because faith and family are the same fabric.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_rights_claimants, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, women_rights_claimants, payer).

% Inherit the settlement: contextualist teaching lets members keep communal belonging while family rules change around them, but they carry the friction — sermons that contradict the law their grandparents knew, marriage contracts drafted under contested standards, and suspicion from neighbors aligned with the other reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, lay_muslim_communities, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, lay_muslim_communities, payer).

% Hold chairs, muftiates, and endowments whose prestige rests on applying the verses as transmitted. Contextualist adoption removes discretionary power over women's cases from their councils, reroutes deference to rival credentialed interpreters, and declares the methods they control historically bounded. They retain enough institutional weight to sit on reform committees, slow implementation, and trade acquiescence for preserved prerogatives.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_religious_elites, payer,
    institutional, generational, constrained, continental).

% Adjudicate inheritance, testimony, and marital disputes under codified rules descended from the classical readings. Each contextualist statute narrows their docket discretion and exposes their rulings to appeal on equity grounds; judges must either adopt the new methodology or defend the old one under supervisory review.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_sharia_courts, payer,
    institutional, generational, constrained, national).

% Work in secular rights frameworks and international bodies. They argue that any regime routing women's rights through authorization by divine text — however egalitarian the method — leaves the underlying architecture of subordination intact and reproduces gatekeeping in scholarly form. Their objection is not admissible inside the intra-Muslim legitimacy conversation, where authority must trace to revelation; they press their case through UN reviews and domestic equality litigation instead.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, secular_feminist_critics, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, reformist_jurists).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared method by which Muslim societies can change scripture-anchored family law while remaining inside revealed-text legitimacy: historical contextualization plus purposive (maqasid) reasoning lets legislatures, courts, and communities revise inheritance, testimony, and marital rules without forcing a choice between apostasy and accepted injustice.
% TRANSFER_FUNCTION: Moves interpretive authority — and the status, appointments, and funding attached to it — from traditionally credentialed jurists to reformist scholars and NGO intermediaries; moves material entitlements (inheritance shares, witness weight, marital obligations) toward women wherever the reading is adopted; moves the costs of legitimacy conflict onto communities and onto women who assert the new claims.
% ABSENT_VOICES: Secular feminist critics are structurally outside the conversation, since their premise (that text-mediated authorization itself is the problem) cannot be voiced within a framework where legitimacy requires tracing to revelation. Uncredentialed laywomen who read the verses for themselves carry no interpretive standing. Ordinary worshippers attached to the transmitted readings are addressed as objects of re-education rather than participants.
% DISAPPEARANCE_RATIONALE: If the contextual-egalitarian reading vanished overnight, family-law reform across Muslim-majority jurisdictions would lose its primary within-tradition warrant: reform projects would stall back into transmitted rules, or migrate to abrogation arguments and secular codification routes that carry far higher schism and backlash risk. State reform commissions, NGO programs, and the scholarly networks built on maqasid argument would lose their organizing framework simultaneously.
% FOUNDING_PROBLEM: Built to solve the collision between revealed family law and modern equality norms — first under nineteenth-century colonial codification pressure, then under post-independence constitutional equality guarantees, and most recently under CEDAW-era treaty scrutiny — without splitting the community or abandoning the text.
% FOUNDING_PROBLEM_CORROBORATION: State family-law commissions in several jurisdictions adopted maqasid arguments under treaty-reporting pressure, corroborating that the collision is unresolved; traditionalist academies concede the equality-pressure problem while disputing the solution; UN treaty-body reviews and domestic equality litigation document the persistent gap between constitutional guarantees and personal-status law. Corroboration therefore exists from seats outside the reformist beneficiary set.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.52: moderate by construction of the reading — the material stakes (inheritance shares, witness weight, marital obedience) remain real wherever transmitted rules still govern, and the remedy adds a certification toll (claims succeed when a recognized scholar vouches for the maqasid argument), while the reading's own framing caps perceived illegitimacy because the rules are presented as situational advances rather than timeless ordinances. Suppression 0.56: enforcement is real but not totalizing — the reading must be actively defended against transmitted-rule application in courts, against accusations of innovation (bid'ah) and Westernization from establishment bodies, and against rival reform mechanisms; yet the literal reading remains fully lawful and practiced in much of the world, so alternatives are pressured, not eliminated. Theater ratio 0.38: a growing share of activity is performative — conference cycles, dueling fatwa collections, translation and report-writing industries that restate the same arguments for new funders — while core doctrinal work continues. Accessibility collapse 0.40: alternatives do NOT collapse; the transmitted reading, abrogation arguments, and secular exit all remain accessible, which is precisely why enforcement effort stays elevated. Resistance 0.60: sustained institutional resistance from religious establishments, bar associations of classical training, and populist traditionalist movements. Measurement series run on ONE shared grid (t = 0, 25, 50, 75, 100, 125, 150) so every metric is authored at every examined point; trajectories are monotonic, not cyclical — extraction and theater rise together as the movement institutionalizes (stakes grow with adoption; the NGO-conference complex grows with the gap it documents), and the suppression requirement rises as enforcement must reach further into courtrooms and curricula. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural facts. From the reformist jurist's seat the arrangement is fidelity completed — revelation's own purpose finally governing its situated commands. From the displaced elite's seat the same structure is usurpation dressed as piety: a rival credential class seizing the microphone. From women's seat it is ambivalent: enforceable claims arrive, but only through a mediator, and asserting them prices social belonging. From the secular critic's outside seat the entire framework is gatekeeping relocated. The engine derives these divergent per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them. Coalition note: individually powerless claimants have demonstrated coalition capacity (transnational faith-based feminist networks coordinating litigation and treaty-shadow-reporting), which raises the effective resistance the payer seats face without changing any individual's exit position.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Reformist jurists sit nearest the beneficiary end: the reading subsidizes their authority and they administer its application. Rights-based NGOs benefit incidentally and can leave. Lay communities sit near symmetric: they receive communal coherence and pay conflict friction. Patriarchal elites and traditional courts are declared victims/payers: the reading is built, in part, out of their discretionary power. Two overrides correct derivations the structural data alone gets wrong. First, women_rights_claimants: the beneficiary declaration alone would derive d near 0.0 (full subsidy), but they also bear the enforcement costs — social sanction, mediation dependence, the burden of litigating reinterpretation — so d is overridden to 0.35, encoding net-beneficiary-with-real-costs. Second, the institutional atom (patriarchal_religious_elites, traditional_sharia_courts): the payer declaration alone would derive d near 1.0, but both seats retain institutional positions and partially capture the reform process (elite bodies absorb reformist language, sit on drafting committees, trade acquiescence for preserved prerogatives), so d is overridden to 0.65 — paying, but with partial recapture. Scope amplification applies modestly at the global scopes carried by the interpreter class and claimant population.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling revealed family law with equality norms without schism — is live, so no mandate-obsolescence verdict is available and none is claimed. The tangled_rope classification prevents two opposite mislabels. Reading the structure as pure coordination (rope) would erase the certification toll: women's claims are realizable only through credentialed mediation, and interpretive precedence is a collected good, which is extraction riding on coordination. Reading it as pure extraction (snare) would erase what the structure genuinely solves: without a within-tradition method, reform either stalls or forces communities toward rupture, and women's fallback under the transmitted reading is materially worse. The classification holds both truths in one structure. The identity-lock dynamic is constitutive here: the interpreter class cannot exit the tradition without dissolving its own authority, and the claimant population cannot exit the community without dissolving family and faith together — if either identity frame broke (a direct-access norm for textual meaning, or normalized plural belonging), the gatekeeping component would evaporate and the residue would be plain coordination cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel quranic_gender_verses; how would instantiating a sibling reading change the victim set and effective extraction?',
    'Comparative classification of the sibling stories: literal_hierarchical places women in the victim set with materially higher extraction; progressive_abrogation routes change through naskh with a different gatekeeping class. Cross-jurisdiction comparison of which reading is operative where.',
    'Where literal_hierarchical is operative, women remain victims with substantially higher material extraction and this story''s moderate profile does not apply; where progressive_abrogation displaces this reading, the mediation structure changes hands rather than dissolving.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame routing: sibling readings instantiate different constraints with different victim sets and epsilons.').

omega_variable(
    maqasid_mediation_dependency,
    'Does maqasid-based reinterpretation transfer enforceable rights to women directly, or does it substitute credentialed-scholar mediation such that a woman''s claim exists only when a recognized interpreter certifies it?',
    'Track personal-status litigation outcomes where claimants advance contextualist arguments with and without recognized scholarly sponsorship; compare grant rates and procedural standing.',
    'If mediation-dependent, a measurable share of the extraction is gatekeeping rent and the payer-seat computation shifts toward the coercive end; if rights transfer directly, the coordination reading strengthens and effective extraction falls toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maqasid_mediation_dependency, empirical, 'Whether the reading''s remedy empowers claimants or relocates gatekeeping.').

omega_variable(
    implementation_gap_trajectory,
    'Will the gap between formal adoption (family-law amendments, codified reforms, treaty ratifications) and applied courtroom practice close within a generation?',
    'Longitudinal coding of personal-status rulings across jurisdictions against statutory equality provisions, controlling for appellate review intensity.',
    'A persistent gap sustains moderate-to-high effective extraction for claimants despite formal beneficiary status; closure collapses extraction toward the inherent cost of running any interpretive system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_trajectory, empirical, 'Formal-versus-applied gap as the main carrier of residual extraction.').

omega_variable(
    historicity_of_progressive_step_premise,
    'Does the historiography support the load-bearing premise that the inheritance, testimony, and guardianship provisions were experienced as progressive improvements within seventh-century Arabian practice?',
    'Independent historiographic reconstruction of pre-Islamic Arabian inheritance, witnessing, and marital practices compared against the verses'' provisions and earliest application.',
    'If the premise fails, the reading loses its distinguishing axiom and its authority collapses toward progressive_abrogation or toward the transmitted reading; if supported, the reading''s interpretive warrant strengthens and its enforcement burden falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historicity_of_progressive_step_premise, empirical, 'Empirical foundation of the historical-contextualization axiom.').

omega_variable(
    legitimacy_conflict_resolution_path,
    'Does the intra-community legitimacy conflict resolve into durable interpretive pluralism (readings coexisting across jurisdictions and schools) or into zero-sum displacement of one reading by another?',
    'Track institutional outcomes: parallel court systems or procedural pluralism indicate coexistence; statutory entrenchment of a single methodology indicates displacement.',
    'Pluralist resolution lowers the suppression requirement over time and stabilizes the tangled-rope profile; zero-sum resolution pushes whichever reading loses toward piton-like theatrical maintenance and the winner toward hardened enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_conflict_resolution_path, conceptual, 'Whether coexistence or displacement governs the kernel''s future.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_ctx_eg_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qgv_ctx_eg_tr_t25, quranic_gender_verses__contextual_egalitarian, theater_ratio, 25, 0.18).
narrative_ontology:measurement(qgv_ctx_eg_tr_t50, quranic_gender_verses__contextual_egalitarian, theater_ratio, 50, 0.22).
narrative_ontology:measurement(qgv_ctx_eg_tr_t75, quranic_gender_verses__contextual_egalitarian, theater_ratio, 75, 0.27).
narrative_ontology:measurement(qgv_ctx_eg_tr_t100, quranic_gender_verses__contextual_egalitarian, theater_ratio, 100, 0.31).
narrative_ontology:measurement(qgv_ctx_eg_tr_t125, quranic_gender_verses__contextual_egalitarian, theater_ratio, 125, 0.35).
narrative_ontology:measurement(qgv_ctx_eg_tr_t150, quranic_gender_verses__contextual_egalitarian, theater_ratio, 150, 0.38).

% Extraction over time
narrative_ontology:measurement(qgv_ctx_eg_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(qgv_ctx_eg_be_t25, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 25, 0.36).
narrative_ontology:measurement(qgv_ctx_eg_be_t50, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(qgv_ctx_eg_be_t75, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(qgv_ctx_eg_be_t100, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(qgv_ctx_eg_be_t125, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 125, 0.5).
narrative_ontology:measurement(qgv_ctx_eg_be_t150, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 150, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(qgv_ctx_eg_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qgv_ctx_eg_su_t25, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 25, 0.34).
narrative_ontology:measurement(qgv_ctx_eg_su_t50, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 50, 0.39).
narrative_ontology:measurement(qgv_ctx_eg_su_t75, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 75, 0.44).
narrative_ontology:measurement(qgv_ctx_eg_su_t100, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 100, 0.49).
narrative_ontology:measurement(qgv_ctx_eg_su_t125, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 125, 0.53).
narrative_ontology:measurement(qgv_ctx_eg_su_t150, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 150, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Qur'an says about women's rights' decomposes into three structurally distinct constraints sharing one kernel. This member (contextual_egalitarian) carries moderate extraction routed through scholarly mediation; literal_hierarchical carries high material extraction with women as victims; progressive_abrogation routes egalitarian outcomes through abrogation doctrine with its own gatekeeping. The upstream member in empirical-confidence terms is literal_historical scholarship on seventh-century practice, which this reading cites as evidence for its situatedness premise; the siblings compete for the same reformist constituency, so edges run both ways. Each file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, powerless, 0.35).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
