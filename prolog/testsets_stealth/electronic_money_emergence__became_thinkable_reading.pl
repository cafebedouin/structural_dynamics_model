% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Thinkability-First Reading of Electronic Money Emergence
 *   domain: economic history/monetary theory/technology studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   electronic_money_emergence: the became_thinkable_reading, which locates
 *   the emergence of digital money at the point where the conceptual
 *   possibility became technically and socially thinkable — prior to, and
 *   independently of, institutional measurement — and models emergence as
 *   gradual diffusion with no threshold event. The standing arrangement under
 *   contest (the epsilon referent) is the periodization itself as it operates
 *   in monetary historiography: a dating framework that connects the history
 *   of cryptography and computing to monetary theory, aligns curricula and
 *   archive priorities, and distributes scholarly capital. The reading
 *   performs real periodization work while asymmetrically concentrating
 *   citation priority, agenda control, and lineage legitimacy on the
 *   conceptual-history seat and its advocacy allies, at the cost of demoting
 *   institutional-statistical and event-dating practice to belated status.
 *   The kernel contest is not folded into this constraint: sibling readings
 *   (first_held_reading, m4_m5_collapse_reading) are separate constraints,
 *   linked via network.affects_constraints, with the committer structure
 *   routed to the kernel_reading_contest omega and the cs_structure reading
 *   relations. Family epsilon profile: this reading's epsilon (0.58) reflects
 *   scholarly-capital asymmetry under open contest; the sibling readings
 *   would center their flows on different apparatuses (archival threshold
 *   practice; the statistical categories themselves). Claim and metrics are
 *   independent: the type is claimed tangled_rope from the authoring seat;
 *   the metrics describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - conceptual_monetary_historians: Primary beneficiary (powerful/identity_locked) — collects citation priority and agenda control; fused with the periodization they administer
 *   - digital_currency_advocates: Secondary beneficiary (organized/mobile) — converts the deep pre-institutional lineage into legitimacy and fundraising narrative
 *   - monetary_statisticians: Primary target (institutional/constrained) — bears demotion of the M-series practice to lagging artifact; dual seat, since they also administer the measurement apparatus the dispute depends on
 *   - threshold_event_historians: Secondary target (moderate/mobile) — event-dating practice invalidated, but can pivot research programs
 *   - early_electronic_money_users: Excluded voice (powerless/trapped) — the people whose money was digitalized; absent from the periodization debate that describes them
 *   - innovation_studies_analysts: Analytical observer (analytical/analytical) — sees the full structure of the periodization contest and its cross-domain generalization claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.58).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.37).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.37).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Thinkability-First Reading of Electronic Money Emergence").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic history/monetary theory/technology studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'f7acb18f-b3f5-42cf-98e5-88cc96e22bf0').
narrative_ontology:cs_kernel_codification('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', distributed).
narrative_ontology:cs_authority_grounding('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', expertise).
narrative_ontology:cs_interpretation_layer_present('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0').
narrative_ontology:cs_reading_relation('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_reading_relation('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', electronic_money_emergence__m4_m5_collapse_reading, forecloses).
narrative_ontology:cs_axiom('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', foundational, emergence_precedes_measurement).
narrative_ontology:cs_axiom_status(emergence_precedes_measurement, holdable).
narrative_ontology:cs_axiom_grounding('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', emergence_precedes_measurement, empirically_contingent).
narrative_ontology:cs_axiom('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', foundational, gradual_diffusion_no_threshold).
narrative_ontology:cs_axiom_status(gradual_diffusion_no_threshold, holdable).
narrative_ontology:cs_axiom_grounding('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', gradual_diffusion_no_threshold, empirically_contingent).
narrative_ontology:cs_axiom('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', secondary, institutional_measurement_is_lagging_indicator).
narrative_ontology:cs_axiom_status(institutional_measurement_is_lagging_indicator, holdable).
narrative_ontology:cs_axiom_grounding('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', institutional_measurement_is_lagging_indicator, empirically_contingent).
narrative_ontology:cs_reference_frame('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', conceptual_innovation_primacy).
narrative_ontology:cs_drift_state('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', contemporary_cbdc_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7acb18f-b3f5-42cf-98e5-88cc96e22bf0', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, digital_currency_advocates).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, conceptual_monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, monetary_statisticians).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, threshold_event_historians).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, conceptual_innovation_precedes_institutional_recognition).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, gradual_diffusion_model_of_technological_emergence).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, institutional_measurement_lags_thinkability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cypherpunk-lineage foundations, crypto policy groups, and fintech think tanks that trace their projects to pre-institutional digital-cash proposals. The thinkability-first dating gives their ventures a genealogy decades deeper than the banking system's categories, which they cite in white papers, fundraising narratives, and policy testimony. Their commitment to the dating is instrumental: if a different narrative paid better they would adopt it, and nothing binds them to this field.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, digital_currency_advocates, beneficiary,
    organized, generational, mobile, global).

% Senior scholars in the history of computing and monetary thought whose monographs, seminars, and edited volumes established the thinkability-first dating. They referee the field's journals, direct doctoral agendas, and decide which archives count as canonical, and their accumulated corpus is organized and dated by the framework they built. Leaving the framework would unravel the framing of their own life's work, so their participation persists regardless of how the contest goes.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, conceptual_monetary_historians, beneficiary,
    powerful, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, conceptual_monetary_historians, agenda_setter).

% Central-bank and statistical-agency staff who define and maintain the monetary aggregates into which electronic money was eventually folded. The thinkability-first dating recasts their categories as belated measurements of something that existed conceptually decades earlier, costing them standing in the dating dispute. They nonetheless still run the measurement apparatus everyone argues about: they define the aggregates, decide what is counted, and control the statistical record. Their institutional mandates bind them to those categories, which they cannot abandon without authorization.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_statisticians, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, monetary_statisticians, agenda_setter).

% Economic historians who date monetary transformations to first instances — first wire transfer, first stored-value card, first institutional holding of dematerialized balances. The thinkability-first dating declares their dated objects misdescribed: what they date is adoption, not emergence. They lose citations and agenda share but can pivot to adjacent programs — payment-system history, adoption diffusion — where their archival skills transfer.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, threshold_event_historians, payer,
    moderate, biographical, mobile, continental).

% Telegraphic remittance customers, early card users, e-cash trial participants, and mobile-money adopters — the people whose money practices the dating dispute describes. The debate over when their money became digital is conducted among scholars and statisticians without collecting their testimony; their own sense of when their money stopped being cash is unrecorded. Their past practice is fixed and they have no seat in the dispute.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, early_electronic_money_users, excluded,
    powerless, immediate, trapped, local).

% Scholars of technological change who study how periodization theses rise, canonicalize, and decay across domains. They take no side in the dating dispute; they track how the thinkability-first framework distributes standing between conceptual and statistical seats, and whether its lag pattern generalizes beyond this case.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, innovation_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__became_thinkable_reading, conceptual_monetary_historians).
narrative_ontology:fixing_cost_class(electronic_money_emergence__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives monetary historiography a single shared periodization: one answer to when digital money began that connects the history of cryptography and computing to monetary theory, aligns research agendas, curricula, and archive priorities, and lets scholars, advocates, and statisticians refer to the same dated narrative.
% TRANSFER_FUNCTION: Moves scholarly capital — citation priority, agenda-setting authority, curriculum share, and lineage legitimacy — from institutional-statistical and event-dating seats toward the conceptual-history seat and its advocacy allies.
% ABSENT_VOICES: The users of early electronic money (telegraphic remitters, card and e-cash trial participants, mobile-money adopters) are absent — the debate over when their money became digital proceeds without their testimony. Also absent: monetary communities outside the core statistical jurisdictions, whose digitalization sequences would not match the canonical lag narrative.
% DISAPPEARANCE_RATIONALE: If the thinkability periodization vanished overnight, the field's dating question would reopen: event-dating historians and the measurement-artifact reading would compete to refill the vacuum, curricula and archive priorities would reorganize around whichever reading won, and advocacy lineages would lose their deep-genealogy narratives. The absence would be immediately visible because seats on both sides of the dispute are organized around the framework.
% FOUNDING_PROBLEM: The founding problem was un-datability: electronic money left no first instance in any institutional record — no first note issued, no first account opened — so neither monetary statistics nor institutional history could locate its emergence. The reading relocated emergence to the point of conceptual-technical thinkability, where the record is datable (Chaum's blind-signature work and the surrounding discourse), converting an unanswerable archival question into an answerable conceptual one.
% FOUNDING_PROBLEM_CORROBORATION: Central-bank archival practice corroborates the founding problem from a victim seat: statistical records contain no first-instance entry for electronic money, which is why the dating question arose at all — the statisticians corroborate the problem while disputing this reading's resolution of it. Innovation-studies scholarship outside the beneficiary set corroborates the broader pattern that conceptual articulation precedes institutional measurement in adjacent payment technologies. No source outside the beneficiary set attests the specific decades-of-lag magnitude; that figure rests on the reading's own dating choices.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the periodization redirects citation, agenda, and lineage legitimacy asymmetrically, but imposes no material coercion — the costs borne by target seats are losses of epistemic standing, not income or liberty. Suppression 0.37: enforcement is refereeing, curricular gatekeeping, and archive-setting — real but bounded; both sibling readings publish and teach. The suppression is structural (editorial and curricular gates) with a modest internalized component as doctoral training canonizes the periodization; at this scale no structural-vs-internalized ambiguity omega is required. Theater 0.33: the periodization does genuine historiographical work, but a rising share of its operation is performative — lineage narratives, anniversary historiography, advocacy genealogy — tracked rising across the interval without approaching functional collapse. Accessibility_collapse 0.40: understanding the thesis does not close alternatives; both sibling readings remain fully available. Resistance 0.60: two organized sibling readings plus the statistical establishment actively contest the periodization. All three tracked series share one time grid (1970-2025, 8 points) so no metric is sampled against another's end-state; suppression_requirement is tracked because this story specifically traces gatekeeping intensity — hardening through canonicalization (1970-2015) and partial easing as constructivist and event-based rivals regained journal space (2015-2025).
 *
 * PERSPECTIVAL GAP:
 *   From the conceptual-history seat the arrangement is a corrective: the field finally dates emergence where the evidence lives, and statistical belatedness is a fact to be explained. From the statistical seat the same structure is a demotion: working measurement categories recast as lagging artifacts by a thesis whose thinkability marker is chosen post hoc. The statisticians' dual seat (they administer the apparatus the thesis demotes) should compute as neither pure beneficiary nor pure target — agenda power in measurement, target position in the dating narrative. Inter-institutional dynamics: the dispute runs between the scholarly field (which enforces the periodization through refereeing) and the central-bank statistical apparatus (which controls the record the periodization is measured against); each side's exit is structured by its institution — scholars can found journals, statisticians cannot abandon mandates. Same-level lateral dynamics: threshold-event historians and conceptual historians sit at the same nominal discipline level but diverge sharply on exit — the event historians are mobile (their archival skills transfer to adoption-diffusion programs), while the senior conceptual historians are identity-locked: their corpus is dated and organized by the periodization, so abandoning it would unravel their own life's work. The engine computes these per-seat differences from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: digital-currency advocates (declared beneficiary, mobile exit) derive near the beneficiary end — the periodization subsidizes their legitimacy at low cost and they can exit if it stops paying. Conceptual historians (declared beneficiary, identity_locked) also derive near the beneficiary end, but their lock makes the asymmetry self-reinforcing rather than fragile: they cannot exit even if the thesis turns costly. Targets: monetary statisticians (declared victim, constrained exit, institutional power) derive near the full-target end — they bear the demotion and cannot abandon the categories their mandates require; threshold-event historians (declared victim, mobile) derive mid-range — real costs, real exit. The excluded users' seat has no beneficiary/victim declaration, so derivation would fall to the power-atom fallback; the override (powerless, 0.55) records that their stake is near-symmetric — their practices are narrated by the dispute, not taxed by it. Receipt: the concentrated gains land on the senior conceptual-history seat (citation priority, editorships, agenda control), while advocate gains are diffuse legitimacy rather than captured capital — hence gain_flow names that seat rather than diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — the un-datability of electronic money's emergence recurs with each wave (mobile money, crypto, CBDC) — so no mandatrophy declaration. The tangled_rope claim prevents two mislabelings: reading the periodization as pure coordination would hide the citation-and-agenda asymmetry its gatekeeping maintains; reading it as pure extraction would erase the genuine dating work the field needed. The receipt surface marks the drift risk: gain_flow names the senior conceptual-history seat and fixing_cost is prohibitive — for the seat that could replace the periodization, replacement costs their accumulated corpus and standing, which exceeds any benefit of fixing, and for the discipline, re-dating the entire narrative is a generation-long project. A captured periodization whose gatekeepers cannot afford to abandon it is the trajectory to watch. R5 mismatch check: founding_problem_status live x disappearance_verdict world_rearranges — no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_date_ambiguity,
    'When, concretely, did digital money become technically and socially thinkable — telegraphic money (1870s), credit-card clearing (1950s), Chaum''s blind signatures (1982), or cypherpunk discourse (1990s)?',
    'Comparative archival study dating first articulations of dematerialized money across technical literature, fiction, and practitioner discourse, set against the dates of institutional statistical recognition.',
    'An early date widens the measurement lag and strengthens this reading against the threshold-event sibling; a late date compresses the lag toward the measurement-artifact sibling and weakens the decades-of-lag claim on which the reading''s law-like presentation rests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_date_ambiguity, empirical, 'The reading''s emergence date is sensitive to which thinkability marker is chosen; each candidate shifts the date by decades.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (became_thinkable_reading) of the kernel electronic_money_emergence — what would the sibling readings (first_held_reading, m4_m5_collapse_reading) change structurally, and where is the disagreement located?',
    'No resolution available inside this reading; the contest is located in the direction of the concept-measurement relation and the shape of emergence (gradual vs. threshold vs. artifact). Resolution would require the field to converge on a single definition of emergence, which the distributed kernel currently prevents.',
    'Adopting first_held makes central-bank archivists the agenda-setters of emergence and dissolves the measurement-lag victim structure; adopting m4_m5_collapse inverts the polarity entirely — statisticians become creators rather than laggards, reclassifying this story''s victim seats as beneficiaries and this reading''s foundational dating as retrojection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: the sibling readings would restructure the beneficiary/victim polarity of this story wholesale; the disagreement is located in the concept-measurement direction and the gradual-vs-threshold shape of emergence.').

omega_variable(
    extraction_vs_normal_contestation,
    'Is the measured asymmetric flow of scholarly capital genuine asymmetric extraction, or the ordinary contestation any periodization thesis generates in an open discipline?',
    'Compare concentration of citation, agenda control, and curriculum share under this periodization against counterfactual plural-periodization regimes in adjacent historical subfields.',
    'If ordinary contestation, effective extraction falls toward coordination-cost levels and the constraint reclassifies toward pure coordination; if concentrated, the tangled-rope reading holds and drift toward harder capture becomes the risk to monitor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_normal_contestation, conceptual, 'Whether the capital asymmetry the periodization maintains exceeds the baseline rivalry of scholarly fields.').

omega_variable(
    gradualism_generalization,
    'Does the claim that measurement lags thinkability by decades generalize across technologies, or is it an artifact of electronic money''s unusually long institutional latency?',
    'Cross-domain comparison in innovation studies (electrification, credit cards, mobile money) measuring thinkability-to-measurement intervals.',
    'If it generalizes, the reading''s law-like presentation approaches a genuine structural regularity and faces mountain-style natural-law certification pressure; if case-specific, the law-like framing is rhetorical cover and the false-natural-law signature should fire.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gradualism_generalization, empirical, 'Whether the reading''s general claim about emergence is a structural regularity or case-specific rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement_basis(elec_tr_t1970, observed).
narrative_ontology:measurement(elec_tr_t1982, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement_basis(elec_tr_t1982, observed).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(elec_tr_t1990, observed).
narrative_ontology:measurement(elec_tr_t1998, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement_basis(elec_tr_t1998, observed).
narrative_ontology:measurement(elec_tr_t2008, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement_basis(elec_tr_t2008, observed).
narrative_ontology:measurement(elec_tr_t2015, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(elec_tr_t2015, observed).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement_basis(elec_tr_t2020, observed).
narrative_ontology:measurement(elec_tr_t2025, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2025, 0.33).
narrative_ontology:measurement_basis(elec_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement_basis(elec_be_t1970, observed).
narrative_ontology:measurement(elec_be_t1982, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1982, 0.26).
narrative_ontology:measurement_basis(elec_be_t1982, observed).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement_basis(elec_be_t1990, observed).
narrative_ontology:measurement(elec_be_t1998, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1998, 0.42).
narrative_ontology:measurement_basis(elec_be_t1998, observed).
narrative_ontology:measurement(elec_be_t2008, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement_basis(elec_be_t2008, observed).
narrative_ontology:measurement(elec_be_t2015, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement_basis(elec_be_t2015, observed).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(elec_be_t2020, observed).
narrative_ontology:measurement(elec_be_t2025, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(elec_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement_basis(elec_su_t1970, observed).
narrative_ontology:measurement(elec_su_t1982, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1982, 0.17).
narrative_ontology:measurement_basis(elec_su_t1982, observed).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement_basis(elec_su_t1990, observed).
narrative_ontology:measurement(elec_su_t1998, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement_basis(elec_su_t1998, observed).
narrative_ontology:measurement(elec_su_t2008, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2008, 0.37).
narrative_ontology:measurement_basis(elec_su_t2008, observed).
narrative_ontology:measurement(elec_su_t2015, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement_basis(elec_su_t2015, observed).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement_basis(elec_su_t2020, observed).
narrative_ontology:measurement(elec_su_t2025, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2025, 0.37).
narrative_ontology:measurement_basis(elec_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% Constraint family: electronic_money_emergence decomposes into three readings of one kernel because the label 'when did digital money emerge' covers structurally distinct claims — a gradual concept-first diffusion (this story), a single institutional threshold (first_held), and a measurement artifact (m4_m5_collapse). Each reading carries its own epsilon, beneficiaries, and victims: this reading's epsilon (0.58) reflects scholarly-capital asymmetry under open contest, while the siblings would center their flows on archival threshold practices and on the statistical categories themselves. Linked via affects_constraints per the epsilon-invariance decomposition rule; this reading's gradualist, concept-first premise forecloses both siblings within any single framework, so the family is a genuine contest rather than a division of labor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__became_thinkable_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
