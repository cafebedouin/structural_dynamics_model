% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Legitimacy Kernel — Continuity Reading (Post-1928 Script Severance)
 *   domain: political linguistics / state formation / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   orthographic legitimacy: what makes a script regime authoritative for a
 *   polity. The 1928 Turkish script revolution replaced the Ottoman Arabic
 *   script with a Latin-derived alphabet, and the standing arrangement under
 *   contest is the post-reform regime: an entire public sphere conducted in
 *   an orthography that is structurally opaque to the pre-1928 corpus. The
 *   continuity_reading holds that a script regime is legitimate insofar as it
 *   preserves access to the historical, religious, and literary inheritance;
 *   assessed by that light, the standing arrangement's defining feature is a
 *   seal — post-reform generations cannot read their grandparents' letters,
 *   title deeds, gravestones, endowment charters, religious commentary, or
 *   the Ottoman literary and historiographical archive without years of
 *   specialist training almost no one receives. The manifest's expected delta
 *   is followed: low epsilon, mountain-like, emphasizing loss rather than
 *   extraction. The interval indexes years since the 1928 reform (T0 = 1928,
 *   T95 = 2023). The claim/metrics gap is deliberate and load-bearing: this
 *   story CLAIMS mountain (the incompatibility is a structural fact of
 *   encoding; for any seated agent the barrier is absolute) while the
 *   authored metrics honestly describe a decay-to-inertia profile —
 *   enforcement decaying, extraction decaying, theatrical commemoration
 *   rising. The engine computes per-seat verdicts from the structural data;
 *   where they diverge from this claim, that divergence is the measurement
 *   the corpus exists to take.
 *
 * KEY AGENTS:
 *   - post_reform_generations: primary target (moderate/constrained) — bears the sealed-archive condition as a permanent background fact of their literacy
 *   - religious_tradition_communities: secondary target (organized/identity_locked) — lost textual continuity while retaining oral authority
 *   - ottomanist_scholars: target-gatekeeper hybrid (moderate/constrained) — the only payers who can read the seal, paid in scarcity for mediating it
 *   - republican_educational_authority: agenda-setter (institutional/arbitrage) — administers the orthography without collecting from the severance
 *   - ottoman_heritage_movements: mobilized aggrieved class (organized/constrained) — campaigns on the loss and draws energy from its persistence
 *   - international_script_policy_analysts: analytical observer — comparative seat, no standing inside the seal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.12).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.28).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.26).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Legitimacy Kernel — Continuity Reading (Post-1928 Script Severance)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political linguistics / state formation / commitment systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '0304d00b-5ea9-4d90-9dbd-57ec99da7c1e').
narrative_ontology:cs_kernel_codification('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', distributed).
narrative_ontology:cs_authority_grounding('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', distributed).
narrative_ontology:cs_reading_relation('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', orthographic_legitimacy_kernel__modernist_reading, forecloses).
narrative_ontology:cs_reading_relation('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', foundational, orthographic_legitimacy_requires_tradition_access).
narrative_ontology:cs_axiom_status(orthographic_legitimacy_requires_tradition_access, holdable).
narrative_ontology:cs_axiom_grounding('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', orthographic_legitimacy_requires_tradition_access, deontological).
narrative_ontology:cs_axiom('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', secondary, severed_transmission_is_unrestorable_loss).
narrative_ontology:cs_axiom_status(severed_transmission_is_unrestorable_loss, holdable).
narrative_ontology:cs_axiom_grounding('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', severed_transmission_is_unrestorable_loss, empirically_contingent).
narrative_ontology:cs_reference_frame('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', continuous_ottoman_scriptural_transmission).
narrative_ontology:cs_drift_state('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', contemporary_post_reform_regime, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('0304d00b-5ea9-4d90-9dbd-57ec99da7c1e', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, religious_tradition_communities).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, ottomanist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, ottoman_heritage_movements).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, script_incommensurability_thesis).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, irreversibility_of_transmission_rupture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born after 1928 into Latin-script schooling: literate in the official orthography, unable to read the Ottoman corpus — family letters, title deeds, gravestones, endowment charters, religious books, the literary and historiographical archive. Access requires years of specialist paleography available to few; translations reach a fraction of the corpus and carry none of its ritual or legal authority. Exit would mean acquiring a second script as an adult against the grain of every institution; most bear the seal as a permanent background condition and discover its depth at probate, at the graveside, or in the library.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    moderate, biographical, constrained, national).

% Professional custodians of the sealed corpus: the only payers who can read the archive, after paying years of specialist training for the privilege. The seal burdens them — shrinking audiences, aging cohorts, endless transliteration drudgery — and simultaneously yields a thin scarcity premium, since courts, ministries, foundations, and families needing the past must come through a bottleneck of a few hundred practitioners. Leaving the field forfeits the training; staying means mediating a national inheritance through themselves.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottomanist_scholars, payer,
    moderate, biographical, constrained, global).

% Communities whose devotional and legal life ran on Arabic-script texts: Ottoman commentaries, waqf charters, annotated Qur'an copies, cemetery epigraphy. The seal cut their textual continuity while oral transmission survived, so authority persists but documentation does not — old endowment deeds and court records are illegible to their own members. Acquiring the script is not an exit but a reclamation, fused with religious identity; substituting translations is experienced as losing the thing itself.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_tradition_communities, payer,
    organized, generational, identity_locked, national).

% The ministry-and-language-institution complex that owns the orthography: curricula, teacher formation, official spelling, textbook approval. It administers the seal without collecting from it; its own officials are products of the same Latin schooling and must hire the experts it certifies to read the state's own archives. At any budget cycle it could reintroduce dual instruction or fund wholesale transliteration; it has preferred commemoration, elective token courses, and pilot digitization projects.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, republican_educational_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Political and cultural movements organized around the lost continuity: they mourn the seal publicly, campaign for restoration, and draw recruitment and electoral energy from its persistence. Their leadership is itself a product of Latin-script schooling; the promised restoration always waits for the next election, and the distance between promise and delivery renews with every cycle.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottoman_heritage_movements, payer,
    organized, biographical, constrained, national).

% Comparative researchers of script reform who study the Turkish case alongside Uzbek, Serbian, Hebrew, and Mongolian cases. They document the costs of severance and the conditions under which dual-script regimes survive, and publish assessments no domestic seat is obliged to read; they hold no standing inside the seal and bear none of its costs.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, international_script_policy_analysts, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing Latin-script orthography coordinates mass written communication: one standard for schooling, printing, administration, and commerce across the whole population. Within this reading's instantiation that coordinating core belongs to the instrumentalist sibling's ledger; the element isolated here — the incompatibility that seals the pre-1928 corpus — solves no coordination problem of its own and is the uncaptured remainder of a coordination success recorded in a sibling file.
% TRANSFER_FUNCTION: Predominantly a non-transfer: embodied access to the pre-1928 textual inheritance is removed from the post-reform population without arriving anywhere — destroyed rather than delivered. Residual flows are thin and derivative: a scarcity premium to the small Ottomanist expert class, and mobilizable grievance to heritage politics; neither constitutes receipt of the destroyed value itself.
% ABSENT_VOICES: The tradition-bearers of the pre-1928 reading publics — the ulema, calligraphers, and Ottoman literati — are dead and enter only as the archive they left. Their successors (traditional religious communities, provincial families holding Ottoman deeds, letters, and inscriptions, Arabic-script printers ruined by the early bans) entered policy conversations late and marginally. The reform-era conversation ran among state planners, linguists, and modernizers; those bearing the access-loss were the objects of policy, not parties to it, and their descendants speak mainly through cultural politics rather than through any seat in the arrangement.
% DISAPPEARANCE_RATIONALE: Were the incompatibility healed overnight — universal functional dual literacy, or complete faithful transliteration joined to living interpretive continuity — the Ottoman archive would reopen to the general public: family papers, endowment deeds, gravestones, religious commentary, and the literary and historiographical corpus would return to circulation. Historiography, religious life, property litigation, and genealogical practice would reorganize around recovered sources, and heritage politics would lose its central grievance. Arrangements demonstrably depend on the seal's persistence.
% FOUNDING_PROBLEM: The standing arrangement descends from the 1928 script revolution, which was built to solve mass illiteracy under the Arabic script, the inefficiency of Ottoman orthography for printing and administration, and the perceived need to attach the new republic to European modernity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside both camps in part: contemporaneous foreign educational missions and demographic surveys attest that mass illiteracy was real and severe. But historians of education independent of both the republican-modernist and continuity positions dispute how much of the measured literacy rise is attributable to script change versus the simultaneous expansion of schooling — so the founding problem is attested, while the necessity of the tradeoff and the attribution of the cure remain disputed from outside the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because the seal transfers almost nothing to anyone: the destroyed value (intergenerational access) arrives nowhere. The decay series (0.42 down to 0.12) tracks the extinction of the only early extraction channels — Latin-script printers protected from Arabic-script competitors by the early bans, and the suppressed scribal professions — which ceased to bite as the Arabic-script economy aged out. Suppression is authored at 0.28 as residual discouragement: the enforcement machinery (printing bans, curricular exclusion, licensing friction) has been progressively dismantled, and the series falls accordingly; suppression_requirement is tracked precisely because enforcement-capacity decay IS this story's dynamic. Accessibility collapse is high (0.78): once a citizen grasps that reading the archive requires adult acquisition of a second script against every institutional grain, the alternative route collapses for all but a specialist few. Resistance is moderate-low (0.26): periodic restoration campaigns, religious objections, and heritage politics achieve token concessions but no reversal. Theater ratio rises monotonically (0.06 to 0.46): as functional enforcement died, commemorative activity grew — anniversary ceremonies, elective token courses, museum heritage display — performing continuity without restoring access. The theatricality is a symptom, not the test: what remains beneath it is a substantive barrier (millions of unread documents, live property and worship consequences), which is why the ratio is authored below the piton-flagging threshold. All three series run on one shared seven-point grid; base_properties scalars equal the T95 endpoints. Boltzmann coordination typing is deliberately omitted: the standing arrangement's genuine coordination function (one standard for mass literacy, print, administration) belongs to the instrumentalist sibling's ledger under the epsilon-invariance decomposition; this story carries the uncaptured remainder, which solves no coordination problem of its own.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats should compute a mountain-like fate: targets with constrained or identity-locked exits facing a barrier no one defends and no one profits from — pure loss. The agenda-setter seat should compute differently: the ministry holds arbitrage-grade exit (it writes the curricula) and could initiate repair at any budget cycle, so at that seat the structure may compute with inertial-administrative coloring — the administrator-could-fix-it-but-cost-exceeds-what-it-bears asymmetry. The scholars' seat is structurally ambivalent: locked out of their own audience yet gatekeepers of the only crossing, they experience the seal as both burden and professional moat. These divergences are computed by the engine from the authored structural data; this story does not reconcile them to the mountain claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared victims (post_reform_generations, religious_tradition_communities, ottomanist_scholars) derive directionality near the full-target end, amplified by their exit profiles: the general class is constrained, the religious communities identity-locked (their relation to the texts is constitutive — translations do not carry ritual or legal authority, so substitution is experienced as losing the thing itself). No beneficiary is declared anywhere in the structural arrays: under this reading the severance has no collector, which is the operational meaning of 'loss rather than extraction.' The agenda-setting authority is not an array member, so the canonical power-atom fallback would guess its position; the directionality override places institutional seats at 0.45 because the ministry administers without collecting and bears its own compliance costs (its officials are graduates of the same Latin schooling and must hire certified experts to read the state's own archives) — nearer symmetric than any beneficiary-leaning default. Spatial scope is national: verification of true access costs is domestically feasible, so no large-scope amplification is warranted.
 *
 * MANDATROPHY ANALYSIS:
 *   The reform's enforcement mandate completed its transformative work within roughly a generation: by mid-century there was no longer a functioning Arabic-script public to suppress, and the mandate that justified bans and purges died of success. The measurement series registers this as falling suppression and rising theater — the classic signature of a spent mandate kept alive performatively. But the harm the continuity reading names is not the mandate and has not expired: the severance renews with every birth cohort. Classification guards against both mislabels: reading the seal as a snare would require naming a capturer, and none exists (the receipt surface affirmatively records diffuse); reading it as pure coordination erases the declared victims' real, unrecompensed losses. The mountain claim locates the structure as fixity wearing a dead enforcement shell — and the shell's mandatrophy is resolved even though the fixity is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_committer_routing,
    'Which reading of the orthographic_legitimacy_kernel does this story instantiate, and what would each sibling reading change structurally?',
    'Kernel registry lookup matching the constraint_id suffix to the declared reading, then comparison against the sibling files: the instrumentalist_reading declares beneficiaries (republican state administration, mass-literacy constituencies) and higher epsilon; the modernist_reading counts the rupture as vindication and the severed corpus as sunk cost. This file is the continuity_reading only.',
    'Misrouting would import the siblings'' beneficiary structures into this file, inflating epsilon and triggering false-summit machinery; correct routing keeps this story loss-framed with no declared beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_committer_routing, conceptual, 'Committer routing: this file instantiates the continuity_reading; the instrumentalist and modernist readings are separate constraints with separate epsilon values.').

omega_variable(
    fixity_vs_constructed_severance,
    'Is the seal over the pre-1928 corpus a structural fixity (two distinct orthographies are mutually opaque without years of training, so the severance reproduces itself once established), or a maintained construction that a sustained dual-script instruction policy could reverse?',
    'Comparative policy natural experiments (Serbian dual-script maintenance, Hebrew revival, Uzbek script reversals) combined with intergenerational transmission modeling: if sustained dual instruction restores functional public access within two generations, the seal is constructed; if transmission-chain losses (interpretive communities, scribal conventions, oral commentary) prove unrestorable regardless of instruction, fixity stands.',
    'Constructed origin would push reclassification toward transitional-support categories with restoration obligations and a defensible sunset logic; confirmed fixity certifies the mountain-like reading with a permanent victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixity_vs_constructed_severance, empirical, 'Whether the severance is an irreducible structural consequence or a policy-maintained condition.').

omega_variable(
    residual_suppression_source,
    'Is the residual suppression (0.28 at interval end) composed of active barriers — curricular exclusion, publication economics, social sanction against Arabic-script literacy — or merely the inertial absence of demand?',
    'Demand-elasticity test: subsidize Ottoman-script courses nationally and measure uptake against cost; flat uptake under subsidy indicates inertial absence of demand, while surging uptake against lingering friction indicates active barriers still bind.',
    'Active barriers would imply a suppression-residue component deserving heavier weighting and closer scrutiny of who maintains them; inertial confirmation supports the fixity reading with the enforcement shell treated as dead rather than suppressed-alive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_suppression_source, empirical, 'Whether remaining suppression is structural force or decayed-demand artifact.').

omega_variable(
    terminal_value_of_traditional_access,
    'Is preserved access to the pre-1928 corpus a terminal value grounding real obligations, or instrumentally discountable against measured literacy and administrative gains?',
    'Not resolvable by measurement: turns on whether the continuity reading''s deontological premise (an obligation of transmission owed to predecessors and descendants) is accepted. Revealed only in constitutional-cultural argument and long-run civic valuation.',
    'If discounted, the severed generations'' loss dissolves into ordinary transition cost, epsilon trends toward zero, and the victim declaration becomes historically retrospective only; if affirmed, restoration duties strengthen and the cost assessment of fixing tightens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_value_of_traditional_access, preference, 'Whether the continuity reading''s value premise is shared or rejected across seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement_basis(orth_tr_t15, observed).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(orth_tr_t30, observed).
narrative_ontology:measurement(orth_tr_t50, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(orth_tr_t50, observed).
narrative_ontology:measurement(orth_tr_t65, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 65, 0.31).
narrative_ontology:measurement_basis(orth_tr_t65, observed).
narrative_ontology:measurement(orth_tr_t80, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(orth_tr_t80, observed).
narrative_ontology:measurement(orth_tr_t95, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 95, 0.46).
narrative_ontology:measurement_basis(orth_tr_t95, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(orth_be_t15, observed).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(orth_be_t30, observed).
narrative_ontology:measurement(orth_be_t50, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 50, 0.23).
narrative_ontology:measurement_basis(orth_be_t50, observed).
narrative_ontology:measurement(orth_be_t65, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 65, 0.18).
narrative_ontology:measurement_basis(orth_be_t65, observed).
narrative_ontology:measurement(orth_be_t80, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 80, 0.14).
narrative_ontology:measurement_basis(orth_be_t80, observed).
narrative_ontology:measurement(orth_be_t95, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 95, 0.12).
narrative_ontology:measurement_basis(orth_be_t95, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(orth_su_t15, observed).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(orth_su_t30, observed).
narrative_ontology:measurement(orth_su_t50, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(orth_su_t50, observed).
narrative_ontology:measurement(orth_su_t65, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 65, 0.36).
narrative_ontology:measurement_basis(orth_su_t65, observed).
narrative_ontology:measurement(orth_su_t80, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement_basis(orth_su_t80, observed).
narrative_ontology:measurement(orth_su_t95, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 95, 0.28).
narrative_ontology:measurement_basis(orth_su_t95, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the orthographic_legitimacy_kernel per the epsilon-invariance principle. The colloquial label 'the Turkish alphabet reform' bundles three structurally distinct claims, each with its own epsilon, victim set, and classification: the instrumentalist_reading (legitimacy from literacy rates and administrative efficiency — the reform's own justification, upstream, highest empirical confidence about literacy outcomes) ; the modernist_reading (legitimacy from alignment with European modernity and rupture from the Ottoman-Islamic past); and this continuity_reading (legitimacy from preserved access to the inherited corpus), which reads the same standing arrangement downstream as a seal. The upstream instrumentalist settlement shapes the operating environment of both sibling critiques; the continuity and modernist readings place opposite valuations on the identical historical fact. Linkage via affects_constraints keeps contamination propagation within the family; no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
