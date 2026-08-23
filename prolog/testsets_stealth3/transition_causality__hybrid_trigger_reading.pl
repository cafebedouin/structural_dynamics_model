% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Hybrid Trigger Reading of the Bretton Woods Transition Causal Template
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the hybrid-trigger reading of the
 *   transition_causality kernel: the causal account of the Bretton Woods
 *   transition under which structural contradictions — chiefly the Triffin
 *   dilemma's reserve-provision arithmetic — accumulated as a slow-burning
 *   load, but the actual collapse of gold-dollar convertibility required
 *   contingent trigger events (Vietnam-era fiscal expansion and the run of
 *   foreign conversions culminating in French gold demands). The constraint
 *   under classification is not the historical events but the
 *   institutionalized causal template built on this reading: the account
 *   taught, published, refereed, and converted into policy heuristics by the
 *   synthesis complex of journals, textbook authors, and central-bank history
 *   units. Epsilon's referent, per the kernel-reading rule, is that standing
 *   arrangement — the consolidated causal-account regime — assessed by this
 *   reading's own lights; any endorsed successor account is not the referent,
 *   and this reading acknowledges its own complicity in the arrangement it
 *   scores. Claim and metrics are authored independently: the template is
 *   CLAIMED as tangled_rope — a real coordination function (a shared causal
 *   grammar for regime-change explanation) entangled with asymmetric
 *   extraction (publication-space, citation, and mind-share taxes on rival
 *   causal programs, plus a policy-complacency externality the template never
 *   prices) — while the metrics describe observed operation without tuning
 *   toward any predicted engine output. Sibling readings are separate
 *   constraints in the same kernel family, linked via
 *   network.affects_constraints; their structural deltas are routed to omega
 *   variables per the committer-frame rules.
 *
 * KEY AGENTS:
 *   - - central_bank_research_establishments: agenda-setting administrator (institutional/arbitrage) — curates the official account, converts it into surveillance mandates, collects the policy-lesson rents
 *   - - flagship_monetary_history_journals: agenda-setting gatekeeper (institutional/arbitrage) — referees and citation norms; captures submission surplus
 *   - - mainstream_synthesis_historians: primary beneficiary (organized/identity_locked) — collects citations, textbook adoption, advisory standing; exit means devaluing a career's interpretive capital
 *   - - early_warning_framework_builders: secondary beneficiary (institutional/mobile) — legitimacy and founding validation case for surveillance frameworks
 *   - - pure_contingency_historians: primary target (moderate/identity_locked) — demoted to trigger-chapter color; methodological identity bars exit
 *   - - structural_determinist_economists: primary target (moderate/identity_locked) — language absorbed, conclusion stripped; ideological identity bars exit
 *   - - dollar_continuity_scholars: secondary target (moderate/constrained) — periodization files their continuity thesis as closed-question dissent
 *   - - economics_graduate_trainees: dual-positioned mass seat (powerless/trapped) — receives the ready-made scaffold, pays in narrowed hypothesis space
 *   - - global_south_monetary_planners: excluded voice (organized/trapped) — largest real exposure to reserve-currency dilemmas, zero seats in the venues where lessons are allocated
 *   - - methodology_of_science_scholars: analytical observer — sees the full consolidation mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.53).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.4).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Hybrid Trigger Reading of the Bretton Woods Transition Causal Template").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '38be0e06-0275-486a-8c95-44d8927686a1').
narrative_ontology:cs_kernel_codification('38be0e06-0275-486a-8c95-44d8927686a1', distributed).
narrative_ontology:cs_authority_grounding('38be0e06-0275-486a-8c95-44d8927686a1', expertise).
narrative_ontology:cs_interpretation_layer_present('38be0e06-0275-486a-8c95-44d8927686a1').
narrative_ontology:cs_reading_relation('38be0e06-0275-486a-8c95-44d8927686a1', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('38be0e06-0275-486a-8c95-44d8927686a1', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_axiom('38be0e06-0275-486a-8c95-44d8927686a1', foundational, structural_accumulation_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(structural_accumulation_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('38be0e06-0275-486a-8c95-44d8927686a1', structural_accumulation_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('38be0e06-0275-486a-8c95-44d8927686a1', foundational, contingent_trigger_events_actually_required).
narrative_ontology:cs_axiom_status(contingent_trigger_events_actually_required, holdable).
narrative_ontology:cs_axiom_grounding('38be0e06-0275-486a-8c95-44d8927686a1', contingent_trigger_events_actually_required, empirically_contingent).
narrative_ontology:cs_axiom('38be0e06-0275-486a-8c95-44d8927686a1', secondary, trigger_timing_counterfactually_malleable).
narrative_ontology:cs_axiom_status(trigger_timing_counterfactually_malleable, holdable).
narrative_ontology:cs_axiom_grounding('38be0e06-0275-486a-8c95-44d8927686a1', trigger_timing_counterfactually_malleable, instrumental).
narrative_ontology:cs_reference_frame('38be0e06-0275-486a-8c95-44d8927686a1', contradiction_accumulation_trigger_frame).
narrative_ontology:cs_drift_state('38be0e06-0275-486a-8c95-44d8927686a1', post_global_financial_crisis_rediscovery, gap(stable, minor, true)).
narrative_ontology:cs_created_at('38be0e06-0275-486a-8c95-44d8927686a1', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, mainstream_synthesis_historians).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, central_bank_research_establishments).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, early_warning_framework_builders).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, pure_contingency_historians).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, structural_determinist_economists).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, dollar_continuity_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, economics_graduate_trainees).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, economics_graduate_trainees).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_operative_diagnosis).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, contingent_trigger_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research divisions and historical units at major central banks and multilateral monetary institutions commission and curate the official account of the 1971 transition, host the anniversary conferences, and translate the causal template into surveillance mandates and early-warning frameworks. They fund much of the historiography that sustains the template and convene the seminar series where its boundaries are policed. Because their internal authority rests on demonstrated analytic competence rather than on any single explanatory framework, they could abandon or rewrite the template wholesale at real organizational cost but without existential loss; meanwhile the template hands them ownership of the policy lessons drawn from the episode, which is where the template's largest concentrated return pools.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, central_bank_research_establishments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, central_bank_research_establishments, beneficiary).

% Leading general and field journals referee submissions on the transition, set citation expectations through acceptance patterns, and organize special issues around template milestones. Referee networks cluster among the template's practitioners, so papers arguing pure-decision or pure-inevitability accounts face longer review paths and heavier revision demands. The journals capture the resulting submission surplus — more submissions chasing fewer slots at premium rejection rates — and could publish against the template whenever doing so attracted readers, since their revenue does not depend on any single explanatory line.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, flagship_monetary_history_journals, agenda_setter,
    institutional, generational, arbitrage, global).

% Senior scholars whose monographs and textbooks constitute the canonical synthesis. They collect citations, advisory appointments, and classroom adoption for as long as the template remains the field's organizing account. Their accumulated interpretive capital — decades of archival work framed by the hybrid model — is template-specific: recasting themselves into a pure-choice or pure-structure account would devalue their life's output, so departure is professionally unthinkable even where they privately register anomalies.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, mainstream_synthesis_historians, beneficiary,
    organized, biographical, identity_locked, global).

% Surveillance architects in multilateral institutions and allied research groups who build indicator dashboards for reserve adequacy, external imbalances, and safe-asset supply. The hybrid template legitimates their operating stance — watch the slow variables, brace for the fast ones — and supplies the founding historical case their frameworks are validated against. Their modeling skills transfer to any subsequent crisis, so leaving is easy; what they would forfeit is the validation case, not employment.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, early_warning_framework_builders, beneficiary,
    institutional, generational, mobile, global).

% Diplomatic and political historians who attribute the transition to identifiable decisions — the closing of the gold window above all — and to the individuals who made them. Under the template their causal contributions are demoted from explanations to color: decisions become the trigger chapter inside someone else's structural story. Their methodological identity is built on agency-centered archival craft; adopting the template would dissolve the program they trained into, and declining it caps their access to the field's main venues.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, pure_contingency_historians, payer,
    moderate, biographical, identity_locked, continental).

% World-systems, Marxian, and long-wave theorists who argued the gold-exchange standard was doomed by its internal arithmetic long before 1971. The template absorbs their language — contradictions accumulating — while stripping their conclusion, converting inevitability into mere predisposition. Their journals sit outside the main citation circuits, their students face thinner job markets, and their generational intellectual commitments make migration into the synthesis feel like betrayal rather than advancement.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, structural_determinist_economists, payer,
    moderate, generational, identity_locked, global).

% Heterodox international-political-economy and Post Keynesian researchers who contend the 1971 break changed far less than the transition framing implies — the reserve-currency asymmetry persisted, merely unmoored from gold. The template's periodization files their work as dissent on a closed question: publishable, but consigned to specialist outlets and footnote citation. Leaving is possible in principle because their methods generalize to other episodes, but their research agendas, networks, and funding are staked on the continuity claim.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, dollar_continuity_scholars, payer,
    moderate, biographical, constrained, global).

% Doctoral students absorb the template as settled background knowledge in field courses and comprehensive examinations. It hands them a ready-made causal structure for seminar papers and job-market chapters — genuine value — while narrowing the hypothesis space they are trained to explore: nobody teaches them to ask whether the transition happened at all. Individually they hold no lever over curriculum; collectively a cohort's demand moves textbook editions only slowly and at the margin.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, economics_graduate_trainees, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, economics_graduate_trainees, payer).

% Finance ministries and central banks of reserve-scarce economies, whose actual exposure to reserve-currency dilemmas is the largest anywhere in the system. They are absent from the historiographical venues where the transition's lessons are allocated: no panel seats, no refereeing role, no commissioned history. The template's policy conclusions reach them as received wisdom through multilateral channels, and the part of the story their own operating experience speaks to loudest — life under the post-1971 order — never enters the causal record the template administers.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, global_south_monetary_planners, excluded,
    organized, generational, trapped, global).

% Philosophers and sociologists of economics who study how causal templates consolidate in mature fields. They track which counterfactuals the template entertains, which anomalies its interpretive layer absorbs without public revision, and whose testimony the consolidation process never solicited. They neither collect nor pay; their publications document the consolidation mechanism this story describes.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, methodology_of_science_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, central_bank_research_establishments).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared causal grammar for explaining the end of the Bretton Woods order: it partitions causes into slow structural accumulation (reserve-provision arithmetic, liquidity growth against a finite gold stock) versus fast contingent triggers (fiscal expansion, foreign conversion runs), giving historians, macroeconomists, and policy institutions a common vocabulary, a common evidence partition, and a teachable case structure for monetary-regime change generally.
% TRANSFER_FUNCTION: Moves interpretive authority and curricular centrality toward the synthesis complex — journals, textbook authors, central-bank history units — and away from pure-agency and pure-structure programs, which surrender publication space, citations, and graduate mind-share; moves ownership of the episode's policy lessons from decision-focused historians to surveillance-oriented macro institutions.
% ABSENT_VOICES: Reserve-scarce global-south monetary authorities would contest both the causal weighting and the lessons — the template allocates them no seat in any venue where lessons are assigned (authored here as excluded stakeholders). Heterodox continuity theorists are present in print but priced to the conversation's margins. The private deliberations of the 1960s-71 decision-makers enter the record mainly through the synthesis's own archival curation, which selects what counts as the decision record.
% DISAPPEARANCE_RATIONALE: Overnight loss of the shared template would fragment transition studies: every paper would rebuild its own periodization and evidence partition, cross-subfield communication would fall back to translation, graduate training would lose its organizing case, and surveillance institutions would lose the historical validation case their frameworks cite. Rival causal programs would regain visibility quickly, but the coordination the template provides — a common grammar for regime-change causation — would have to be rebuilt before any successor account could serve the same function.
% FOUNDING_PROBLEM: After August 1971, economists and historians faced an explanatory emergency: a monetary order designed as permanent had dissolved within a generation, and the field had to decide simultaneously what killed it and which lesson to carry into the floating-rate era — repair the architecture's flaws, fault the decade's decisions, or both.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist diplomatic historians publishing archival challenges to the synthesis's trigger weighting, and methodologists of economics documenting the template's consolidation path, both attest from outside the beneficiary complex that the causal question remains substantively open and is formally reopened roughly once a decade; the synthesis's own practitioners are the ones attesting settlement, which is precisely why their attestation cannot serve as corroboration here. Periodic institution-commissioned reappraisals of the collapse add a third, partially independent attesting seat.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.53: the template performs real integrative work — a common causal grammar across economic history, international political economy, and macro policy — while taxing every rival program through referee delay, citation discount, curriculum displacement, and the quiet refiling of dissent as commentary on a closed question. Suppression 0.40: alternatives are not prohibited and remain constructible — revisionist monographs appear, heterodox outlets persist — but reaching audiences through them costs placement and students, which is enforcement-by-friction rather than ban. Theater 0.30: the analytic function is live, but a growing share of circulation is ritual — Triffin invoked in introductions without engagement, anniversary essays re-performing the consensus — which is why theater rises steadily while extraction plateaus late in the series. Accessibility_collapse 0.45: rival accounts survive understanding of the template, so alternatives do not fully vanish; resistance 0.50: the causal question is formally reopened roughly once a decade by archival revisionism, sustaining live pressure on the synthesis boundaries. Enforcement is continuous — refereeing networks, curriculum standards, commissioning choices — hence requires_active_enforcement. All three tracked series run on one shared six-point grid (t=0..50 corresponds to 1971..2021) with every metric authored at every point; suppression_requirement is authored because the story specifically tracks enforcement-capacity change: gatekeeping built up through the canonization decades and partially eased as digital circulation opened niche routes, rather than remaining static while only extraction drifted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seats and the payer seats should compute opposite classifications from identical structural facts. From the central-bank and journal seats the template is infrastructure they administer and draw on — coordination they operate; from the three payer seats the same refereeing, commissioning, and curriculum machinery operates as a tax on their programs' survival, applied at exactly the points where they lack exit. Dual-positioned seats diverge internally: graduate trainees hold both the scaffold's benefit and the narrowed hypothesis space, and their paired declarations place them near symmetric. Identity-lock differentiates payers at equal nominal power: contingency historians and structural determinists share the moderate power atom but cannot trade places because each lock is constitutive — methodological identity for the former, ideological identity for the latter — while dollar-continuity scholars retain constrained mobility because their skills generalize beyond this episode. Coalition potential exists among the powerless trainee seat in principle (cohort demand moves textbook editions) but operates on decadal timescales, too slow to protect any individual cohort's exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-directionality seats: synthesis historians sit nearest the beneficiary pole; the central-bank establishments and framework builders sit close behind; journals occupy the agenda-setting end where administration and collection overlap. Victim declarations drive the high-directionality seats: contingency historians, structural determinists, and continuity scholars sit near the target pole, differentiated by exit — identity-locked targets sit nearer full-target than the constrained continuity scholars. Trainees land near symmetric from their paired declarations. No directionality_overrides are authored: every seat's structural relationship is already carried by its beneficiary/victim declarations plus its exit atom, and any override is keyed on a power atom rather than an agent, so correcting one seat would misfire across the heterogeneous agents sharing that atom — three victim groups and one excluded group all hold 'moderate', and two agenda setters and one beneficiary all hold 'institutional'. Where the derivation is silent — the excluded global-south planners, whose relationship to the template is maximum-exposure-without-seat — the silence is documented as signal in absent_voices rather than forced into a fabricated d-value.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy analysis guards this reading against two mislabels. Against snare: the template's coordination function is genuine and prior — a causal grammar for monetary-regime change predates and outruns its extraction of rival-program space — and its targets retain constructible alternatives, so pure-extraction classification would erase the integration value every seat except the payers relies upon. Against piton: the founding problem is not dead — the template's machinery still produces applied diagnoses (safe-asset Triffin redux, eurozone imbalance readings), so its persistence is not theatrical inertia around an expired mandate; the rising theater_ratio measures ritual accretion around a living function, not replacement of function by performance. The mandate has therefore not outlived its function and no resolved-mandatrophy state is asserted. The residual risk this reading concedes is the complacency channel: a grammar that treats triggers as necessary can license waiting-for-the-trigger behavior in surveillance settings — an unpriced externality carried as an omega rather than as a metric, because no historical seat's losses from it are cleanly attributable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_strength,
    'How necessary were the identified triggers — Vietnam-era fiscal expansion and the foreign conversion runs, French pressure foremost — to the actual timing and occurrence of the convertibility collapse?',
    'Quantitative counterfactual modeling of reserve-loss trajectories under no-shock baselines, plus archival reconstruction of the discretion margins available to the conversion-decision makers.',
    'High trigger necessity confirms this reading against the overdetermined sibling; demonstrated sufficiency of the structural load alone collapses this reading toward overdetermination and transfers its victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_strength, empirical, 'Whether the contingent triggers were necessary conditions or mere accelerants of the 1971 collapse.').

omega_variable(
    sibling_instantiation_delta,
    'This constraint is one reading of the transition_causality kernel; what changes structurally if either sibling reading is instantiated instead?',
    'Generate the two sibling stories (transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading) and compare victim sets, epsilon, and computed seat classifications across the family.',
    'Contingent-choice instantiation removes the structural-determinist victim seat and re-centers extraction on decision-archive gatekeeping; overdetermined instantiation removes the contingency-historian seat and raises suppression against counterfactual method.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_instantiation_delta, conceptual, 'Committer structure: reading-relative variation of victim sets and extraction within the kernel family.').

omega_variable(
    synthesis_extraction_genericity,
    'Is the measured extraction — publication, citation, and mind-share taxes on rival causal programs — intrinsic to any successful causal synthesis, or specific rent produced by this template''s enforcement configuration?',
    'Compare extraction and suppression profiles across mature causal syntheses in adjacent fields (plate tectonics, germ-theory adoption) at equivalent consolidation ages.',
    'If generic-to-synthesis, effective extraction sits closer to coordination overhead and the computed classification drifts toward pure coordination; if specific, the tangled-rope reading of this template hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_extraction_genericity, empirical, 'Coordination-floor ambiguity: synthesis-generic cost versus template-specific rent.').

omega_variable(
    complacency_channel_attribution,
    'Does the template''s treatment of triggers as necessary produce real-world waiting-for-the-trigger underinvestment in structural adjustment during quiet accumulation phases, and can any historical actor''s losses be attributed to it?',
    'Policy-process tracing of surveillance institutions'' response thresholds in pre-crisis accumulation windows, benchmarked against counterfactual earlier-adjustment scenarios.',
    'Demonstrated attribution adds a non-academic target seat (adjustment forgone before any trigger arrived), raising effective extraction for policy-facing seats and strengthening the asymmetric-extraction half of the classification; failed attribution leaves the externality unpriced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complacency_channel_attribution, empirical, 'Unpriced complacency externality: whether the causal grammar taxes policy conduct, not just rival scholarship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__hybrid_trigger_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(tran_tr_t0, observed).
narrative_ontology:measurement(tran_tr_t10, transition_causality__hybrid_trigger_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(tran_tr_t10, observed).
narrative_ontology:measurement(tran_tr_t20, transition_causality__hybrid_trigger_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(tran_tr_t20, observed).
narrative_ontology:measurement(tran_tr_t30, transition_causality__hybrid_trigger_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(tran_tr_t30, observed).
narrative_ontology:measurement(tran_tr_t40, transition_causality__hybrid_trigger_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(tran_tr_t40, observed).
narrative_ontology:measurement(tran_tr_t50, transition_causality__hybrid_trigger_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(tran_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__hybrid_trigger_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(tran_be_t0, observed).
narrative_ontology:measurement(tran_be_t10, transition_causality__hybrid_trigger_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(tran_be_t10, observed).
narrative_ontology:measurement(tran_be_t20, transition_causality__hybrid_trigger_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(tran_be_t20, observed).
narrative_ontology:measurement(tran_be_t30, transition_causality__hybrid_trigger_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(tran_be_t30, observed).
narrative_ontology:measurement(tran_be_t40, transition_causality__hybrid_trigger_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(tran_be_t40, observed).
narrative_ontology:measurement(tran_be_t50, transition_causality__hybrid_trigger_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement_basis(tran_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__hybrid_trigger_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(tran_su_t0, observed).
narrative_ontology:measurement(tran_su_t10, transition_causality__hybrid_trigger_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(tran_su_t10, observed).
narrative_ontology:measurement(tran_su_t20, transition_causality__hybrid_trigger_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(tran_su_t20, observed).
narrative_ontology:measurement(tran_su_t30, transition_causality__hybrid_trigger_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(tran_su_t30, observed).
narrative_ontology:measurement(tran_su_t40, transition_causality__hybrid_trigger_reading, suppression_requirement, 40, 0.43).
narrative_ontology:measurement_basis(tran_su_t40, observed).
narrative_ontology:measurement(tran_su_t50, transition_causality__hybrid_trigger_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement_basis(tran_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, information_standard).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, dollar_hegemony_persistence_arrangement).

% DUAL FORMULATION NOTE:
% 'Why Bretton Woods ended' decomposes under the epsilon-invariance principle into three readings with distinct victim sets, distinct epsilon, and distinct enforcement profiles; the colloquial label conflates a modal question (were the triggers necessary?) with a counterfactual question (how broad were the viable alternatives?). This file carries the hybrid-trigger reading; the contingent-choice and overdetermined-collapse siblings are linked as kernel-family members, and dollar_hegemony_persistence_arrangement is coupled as the downstream claim the continuity scholars advance against this reading's transition periodization. Within the family, the overdetermined sibling cites the same structural arithmetic (the Triffin diagnosis) this reading vindicates, and the contingent-choice sibling cites the same trigger-event record — both draw on this reading's evidence partitions while contesting its modal conclusions, so influence runs at the evidence layer while foreclosure holds at the premise layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
