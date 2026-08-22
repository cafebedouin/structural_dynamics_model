% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections — Guarantor-State Supervision Reading
 *   domain: international_law/minority_rights/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the lausanne_minority_protections
 *   kernel: the guarantor_reading, which holds that the 1923 minority clauses
 *   are internationally supervised obligations enforceable through
 *   guarantor-state diplomacy and European human-rights mechanisms, not
 *   solely through host-state domestic interpretation. The constraint modeled
 *   here is the supervisory-enforcement architecture that reading posits — a
 *   pathway, not a substantive catalog of rights. Its history is a lifecycle:
 *   real supervisory function under the League of Nations in the 1920s-30s,
 *   decay after the League's dissolution and during Cold War alliance
 *   politics, and partial revival when individual petition to the European
 *   Court of Human Rights opened in the late 1980s. Per Rule 1, the sibling
 *   readings (restrictive_reading: individual worship only, institutional
 *   matters domestic; expansive_reading: guaranteed functional continuity of
 *   pre-1923 religious governance) are separate constraints with their own
 *   epsilon values and are neither described nor averaged here. The epsilon
 *   referent is the standing arrangement under contest — the
 *   guarantor-supervisory pathway itself as this reading presents it — never
 *   the rights catalog the expansive reading would instantiate. The
 *   claim/metric independence rule applies: scaffold is the structurally true
 *   claim (transitional justification, no enforcement teeth), and the metrics
 *   are authored as descriptively true of actual operation; the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - ecumenical_patriarchate: Primary beneficiary (organized/identity_locked) — institutional continuity depends on the pathway; cannot relocate without dissolving itself
 *   - greek_orthodox_minority_istanbul: Primary beneficiary (powerless/constrained) — individual claimants; shrinking constituency
 *   - muslim_minority_western_thrace: Reciprocal beneficiary (moderate/constrained) — mirror-seat minority inside Greece
 *   - hellenic_foreign_ministry: Agenda-setter and operational beneficiary (institutional/mobile) — runs the diplomatic limb, converts grievances into leverage
 *   - turkish_state: Primary cost-bearer and dormant co-guarantor (institutional/constrained) — bears external scrutiny, holds the rarely-invoked mirror seat
 *   - european_court_human_rights: Adjudicative agenda-setter (institutional/analytical) — determines what the pathway can actually deliver
 *   - minority_foundation_boards: Excluded party (moderate/trapped) — whose grievances are mediated over their heads
 *   - venice_commission: Analytical observer (institutional/analytical) — feeds the interpretive record, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.24).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.12).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections — Guarantor-State Supervision Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/minority_rights/religious_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '7802b2cc-b6de-4bee-bdc9-154d90543c03').
narrative_ontology:cs_kernel_codification('7802b2cc-b6de-4bee-bdc9-154d90543c03', fixed_text).
narrative_ontology:cs_authority_grounding('7802b2cc-b6de-4bee-bdc9-154d90543c03', lineage).
narrative_ontology:cs_interpretation_layer_present('7802b2cc-b6de-4bee-bdc9-154d90543c03').
narrative_ontology:cs_reading_relation('7802b2cc-b6de-4bee-bdc9-154d90543c03', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('7802b2cc-b6de-4bee-bdc9-154d90543c03', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('7802b2cc-b6de-4bee-bdc9-154d90543c03', foundational, international_supervision_of_minority_clauses).
narrative_ontology:cs_axiom_status(international_supervision_of_minority_clauses, holdable).
narrative_ontology:cs_axiom_grounding('7802b2cc-b6de-4bee-bdc9-154d90543c03', international_supervision_of_minority_clauses, conventional).
narrative_ontology:cs_axiom('7802b2cc-b6de-4bee-bdc9-154d90543c03', foundational, guarantor_state_standing_to_invoke).
narrative_ontology:cs_axiom_status(guarantor_state_standing_to_invoke, holdable).
narrative_ontology:cs_axiom_grounding('7802b2cc-b6de-4bee-bdc9-154d90543c03', guarantor_state_standing_to_invoke, conventional).
narrative_ontology:cs_reference_frame('7802b2cc-b6de-4bee-bdc9-154d90543c03', internationally_supervised_reciprocal_regime).
narrative_ontology:cs_drift_state('7802b2cc-b6de-4bee-bdc9-154d90543c03', post_league_echr_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7802b2cc-b6de-4bee-bdc9-154d90543c03', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, greek_orthodox_minority_istanbul).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, muslim_minority_western_thrace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, hellenic_foreign_ministry).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, minority_protection_as_international_concern).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads an ancient church centered in Istanbul whose schools, charitable foundations, and clergy pipeline depend on treaty-era guarantees. Its principal grievance is the closure of its theological seminary on Halki since 1971 under general domestic law. It survives by pressing its case through patron-state advocacy and European litigation; relocating the See would dissolve the canonical identity it exists to preserve, so it stays and advocates.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, ecumenical_patriarchate, beneficiary,
    organized, civilizational, identity_locked, global).

% The remnant of a community exempted from the 1923 population exchange that has shrunk from over a hundred thousand to a few thousand members. Members hold worship, property, and schooling claims and can emigrate — and many have — but each departure thins the community the claims attach to. Individual members rely on the patriarchate and Greek diplomacy to carry their grievances anywhere beyond domestic courts.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greek_orthodox_minority_istanbul, beneficiary,
    powerless, biographical, constrained, national).

% The reciprocal protected minority inside Greece, governed under parallel treaty clauses covering mufti appointments, charitable foundations, and bilingual schooling. It can in principle invoke the same external pathway through Turkish diplomacy, though such invocation is rare and politically charged, and its day-to-day disputes are fought in Greek administrative courts.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, muslim_minority_western_thrace, beneficiary,
    moderate, generational, constrained, regional).

% Operates the diplomatic limb of the arrangement: annual demarches, European Union progress-report language, and raising seminary, foundation, and property questions in every suitable multilateral forum. It converts minority grievances into standing negotiating positions and draws diplomatic material from the file that it can spend across the wider bilateral relationship. It can shift channels freely between bilateral, European, and United Nations venues.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, hellenic_foreign_ministry, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, hellenic_foreign_ministry, beneficiary).

% Hosts the patriarchate and the Istanbul minority and experiences the arrangement as outside scrutiny of domestic arrangements: the seminary closure, foundation-board elections, and property registrations. It holds the mirror guarantor seat for the Western Thrace minority but invokes it sparingly. Denouncing the treaty would carry reputational and alliance-level costs, so it remains inside the framework while resisting the characterization of domestic law as treaty breach, and occasionally trades concessions for diplomatic gains elsewhere.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter).

% Adjudicates individual applications from the protected minorities, reading the treaty-era guarantees alongside Convention articles. Its case law determines what the pathway can actually deliver — including holdings that the treaty text does not by itself create a right to establish minority schools, which narrows the justiciable content. It can declare violations and award damages but cannot compel legislation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_court_human_rights, agenda_setter,
    institutional, generational, analytical, continental).

% Run the community foundations whose property regimes and board elections are the recurring subject of complaint on both sides of the Aegean. Their grievances reach the external pathway only as material filtered through patron-state diplomacy or individual litigation; they hold no seat in the bilateral talks where their issues are raised, deferred, or exchanged, and their endowments are immovable, so they cannot withdraw from the arrangement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, minority_foundation_boards, excluded,
    moderate, generational, trapped, regional).

% The Council of Europe's advisory body on constitutional and legal reform. When consulted on minority-protection frameworks it issues opinions that feed the interpretive record read by the court and by both foreign ministries. It takes no side in the bilateral dispute and collects nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, venice_commission, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, hellenic_foreign_ministry).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a credibility problem: a host state's promises to a protected minority are not believable when only the host's own institutions interpret and enforce them. The arrangement supplies external witnesses, standing, and fora — guarantor-state invocation, originally League supervision, now European human-rights adjudication — that make the commitments observable and contestable by someone other than the promisor.
% TRANSFER_FUNCTION: Moves grievance-handling and agenda-setting power: minority complaints are transferred out of host-state domestic institutions into interstate diplomacy and supranational litigation, and diplomatic leverage is transferred to the guarantor state that operationalizes the file. Little material wealth moves; what moves is jurisdiction, attention, and negotiating currency.
% ABSENT_VOICES: The community foundation boards and lay councils of both protected minorities are absent from the bilateral table where their grievances become negotiating positions; the host states' domestic institutions are likewise absent from the European fora that review their acts. The former would object that their claims are traded rather than adjudicated; the latter that external review overrides domestic democratic process.
% DISAPPEARANCE_RATIONALE: The patriarchate's survival strategy, the Greek ministry's bilateral agenda, and the minorities' litigation posture are all organized around the pathway. Overnight removal would force reversion to purely domestic remedies the communities judge hostile, accelerate the remaining Istanbul community's decline, and strip Greek diplomacy of a standing file — visible rearrangement across multiple seats.
% FOUNDING_PROBLEM: After the First World War and the Greco-Turkish war, the great powers required Turkey — and reciprocally Greece — to commit to protecting the remaining non-Muslim and Muslim populations as the price of recognition and peace, following mass violence and a negotiated population exchange. Guarantor-state diplomacy under League supervision was the enforcement device that made those commitments credible between mutually distrusting states.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European Court of Human Rights case law continues to adjudicate the treaty's scope; Parliamentary Assembly of the Council of Europe resolutions address the seminary and the foundation regimes; United States religious-freedom reporting and academic historiography of the population exchanges document the original problem and its attenuation. The host state disputes that the founding problem remains live, characterizing the residue as ordinary domestic-law matters; the benefiting parties obviously affirm it.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.24 at interval end) because the arrangement chiefly confers standing, venues, and visibility rather than taking resources from anyone it governs; its costs are sovereignty friction and the absorption of minority agency into state channels. Suppression is low (0.12) because the pathway is additive — it adds channels rather than closing them, and possesses no coercive machinery of its own. Accessibility_collapse is low (0.30) because rivals persist: direct Convention litigation, UN treaty bodies, EU accession conditionality, and informal diplomacy all remain available alongside the pathway. Resistance is moderate-substantial (0.55) because the host state sustains a sovereignty objection to external review and has historically delayed compliance with adverse judgments. Theater_ratio (0.52) is the diagnostic center of the story: roughly half of current activity — annual demarches, report language, parliamentary questions — is positional messaging that changes little, while the judicial limb delivers binding but remedy-limited outcomes. The temporal series run on one shared grid (years since 1923: points 0, 15, 30, 45, 60, 75, 90, 100) so every tracked metric is authored at every examined time point. The arc: suppression_requirement tracks enforcement capacity — real supervisory bite under the League (0.18), collapse after 1946 and through the Cold War decades (nadir 0.06 around t=60), partial rebuild via the European judicial limb (0.12 at t=100); theater_ratio rises as function decays (proxy diplomacy replacing enforcement, peaking 0.55 at t=60), dips with the judicial revival, and plateaus; base_extractiveness creeps slowly upward as the pathway's residual value becomes spendable leverage rather than delivered protection. Suppression_requirement is authored because enforcement-capacity change IS this story's dynamics; the scalar base_properties.suppression reflects the endpoint of that series. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the patriarchate and the Istanbul remnant, the pathway is a lifeline — the only forum where domestic outcomes can be revisited. From the Turkish state, the same structure is external review of democratically enacted domestic law, lacking local mandate and applied selectively. From the Greek ministry, it is a working instrument: a standing file that organizes a bilateral relationship. From the European Court, it is ordinary docket — treaty-era guarantees read alongside Convention articles, no more. From the foundation boards, it is mediation conducted over their heads. The engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The three minority-side seats are declared beneficiaries and derive low directionality — the pathway subsidizes them with standing and venue. The two state seats share the institutional power atom, so per-atom directionality overrides could not separate them without flattening the pair; no overrides are authored, and the differentiation is carried by structural data instead: the Greek seat holds a declared secondary beneficiary role and mobile exit (it can arbitrage between bilateral, European, and UN channels), tilting it toward the beneficiary end, while the Turkish seat holds the payer role with constrained exit (denunciation costs) and bears the scrutiny, pushing it toward the target end. The Court sits near-symmetric with analytical exit; the excluded foundation boards sit mid-structure — they would be beneficiaries under the expansive sibling reading, which is precisely the contest the kernel registers. Receipt surface: the pathway's operative yield — standing, agenda control, negotiating material — demonstrably accrues to the Greek ministerial seat, which operationalizes the file; the mirror Turkish seat lies mostly dormant, and minority seats receive outcomes only derivatively. Gain_flow therefore names hellenic_foreign_ministry rather than asserting diffuseness. Fixing is prohibitive on either branch: giving the pathway enforcement teeth requires host-state consent or great-power coercion, and removing it requires treaty denunciation with alliance-level fallout — both exceed any single seat's appetite relative to the benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming scaffold prevents two symmetrical misreadings. Reading the thin diplomatic residue as pure coordination (rope) would credit a mechanism whose enforcement half has been dead since 1946; reading it as pure extraction (snare) would mistake a low-epsilon protective overlay for a predatory one and mislocate the victims. The scaffold claim keeps the transitionality visible: the arrangement's justification was always the post-exchange transition, and the founding_problem_status is authored contested — the populations the founding problem protected have largely departed, yet the residue (seminary, foundations, muftiates) is live enough that no seat safely declares the mandate dead. The mismatch consumer reads contested-status x world_rearranges and finds no dead-mandate capture flag; the theater hump in the measurement series nonetheless documents partial atrophy, and the implicit-sunset omega records that if the League-era supervision was designed to lapse on demographic completion, the surviving diplomatic limb is a scaffold past its term trending toward inertial performance. The classification holds that possibility open rather than baking it in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the lausanne_minority_protections kernel (guarantor_reading). How would instantiating the restrictive_reading or the expansive_reading instead change the structural classification?',
    'Comparative authoring of the sibling stories: the restrictive reading should show a materially different victim/beneficiary structure (protected minorities as parties denied external remedy), and the expansive reading should show higher-stakes claims against the host state (seminary, property, self-administration), shifting epsilon and likely the computed type.',
    'If the restrictive reading dominates interpretation, this pathway''s beneficiary set thins toward nullity and the arrangement trends inertial; if the expansive reading dominates, the pathway becomes the delivery vehicle for high-value claims and its leverage function intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame uncertainty: which reading of the Lausanne kernel the structural data actually instantiates.').

omega_variable(
    enforcement_migration_or_replacement,
    'Does the migration of supervision into the European human-rights system constitute CONTINUATION of the guarantor architecture or REPLACEMENT of it by a distinct constraint?',
    'Doctrinal and behavioral comparison: whether guarantor-state invocation retains independent operative effect alongside Convention litigation, or whether states now act exclusively through the judicial channel — traceable in diplomatic archives and pleading patterns.',
    'If replacement, this constraint''s living content is the diplomatic limb alone, raising theater_ratio further and pushing the residual arrangement toward inertial performance; if continuation, the scaffold persists with a genuinely revived enforcement half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_migration_or_replacement, empirical, 'Whether the European judicial limb is the same constraint continued or a successor constraint.').

omega_variable(
    leverage_instrumentalization_risk,
    'Does routing minority claims through guarantor-state diplomacy convert rights into tradeable geopolitical currency, imposing costs on the very beneficiaries the pathway serves?',
    'Case tracing of episodes where minority issues were raised, deferred, or exchanged against unrelated bilateral objectives (foundation elections, property registrations, seminary reopening timelines) versus episodes where invocation produced durable relief.',
    'If instrumentalization is systematic, the effective burden borne by the minority seats exceeds the authored epsilon, their derived directionality rises above the beneficiary-end values, and the arrangement''s coordination credential weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leverage_instrumentalization_risk, empirical, 'Whether the patron-state channel helps or spends the minorities it represents.').

omega_variable(
    implicit_sunset_ambiguity,
    'Was the League-era supervision designed to lapse upon completion of the population movement — an implicit sunset — such that the surviving architecture is a transitional support past its term?',
    'Travaux préparatoires and League archival practice: whether supervisors treated the minority clauses as provisional pending demographic stabilization or as permanent treaty obligations.',
    'Resolving toward an expired implicit sunset supports treating the residual diplomatic limb as inertial performance maintained by habit rather than function; resolving toward permanence sustains the live-scaffold reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_sunset_ambiguity, conceptual, 'Whether the arrangement carries an unwritten sunset that has already passed.').

omega_variable(
    reciprocity_operational_asymmetry,
    'The guarantor structure is formally reciprocal — each state may invoke the treaty for the minority hosted by the other — but Greece invokes routinely for the patriarchate while Turkey rarely invokes for the Thrace minority. Is the architecture symmetric in form but asymmetric in operation?',
    'Count and weight invocations, demarches, and litigation sponsorships per guarantor seat across the interval; test whether asymmetry tracks minority need or bilateral bargaining position.',
    'Confirmed asymmetry separates the two state seats'' effective directionality despite their identical power atoms — the Greek seat nearer the beneficiary end, the Turkish seat nearer the target end — sharpening the per-seat divergence the engine computes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_operational_asymmetry, empirical, 'Whether the reciprocal guarantee operates symmetrically in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_guarantor_tr_t0, lausanne_minority_protections__guarantor_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t0, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t15, lausanne_minority_protections__guarantor_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t15, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t30, lausanne_minority_protections__guarantor_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t30, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t45, lausanne_minority_protections__guarantor_reading, theater_ratio, 45, 0.46).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t45, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t60, lausanne_minority_protections__guarantor_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t60, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t75, lausanne_minority_protections__guarantor_reading, theater_ratio, 75, 0.44).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t75, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t90, lausanne_minority_protections__guarantor_reading, theater_ratio, 90, 0.49).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t90, observed).
narrative_ontology:measurement(lausanne_guarantor_tr_t100, lausanne_minority_protections__guarantor_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement_basis(lausanne_guarantor_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lausanne_guarantor_be_t0, lausanne_minority_protections__guarantor_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t0, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t15, lausanne_minority_protections__guarantor_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t15, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t30, lausanne_minority_protections__guarantor_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t30, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t45, lausanne_minority_protections__guarantor_reading, base_extractiveness, 45, 0.19).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t45, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t60, lausanne_minority_protections__guarantor_reading, base_extractiveness, 60, 0.2).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t60, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t75, lausanne_minority_protections__guarantor_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t75, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t90, lausanne_minority_protections__guarantor_reading, base_extractiveness, 90, 0.23).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t90, observed).
narrative_ontology:measurement(lausanne_guarantor_be_t100, lausanne_minority_protections__guarantor_reading, base_extractiveness, 100, 0.24).
narrative_ontology:measurement_basis(lausanne_guarantor_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_guarantor_su_t0, lausanne_minority_protections__guarantor_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t0, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t15, lausanne_minority_protections__guarantor_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t15, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t30, lausanne_minority_protections__guarantor_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t30, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t45, lausanne_minority_protections__guarantor_reading, suppression_requirement, 45, 0.07).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t45, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t60, lausanne_minority_protections__guarantor_reading, suppression_requirement, 60, 0.06).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t60, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t75, lausanne_minority_protections__guarantor_reading, suppression_requirement, 75, 0.09).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t75, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t90, lausanne_minority_protections__guarantor_reading, suppression_requirement, 90, 0.11).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t90, observed).
narrative_ontology:measurement(lausanne_guarantor_su_t100, lausanne_minority_protections__guarantor_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(lausanne_guarantor_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Lausanne minority protections' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story (guarantor_reading) authors the ENFORCEMENT-LOCUS claim with low epsilon (a supervisory pathway that confers standing and venue). The restrictive_reading authors the SUBSTANCE-NARROWING claim (worship only; institutional matters domestic) — measuring it would show the host state's domestic regime bearing the full interpretive load, with the protected minorities as denied claimants. The expansive_reading authors the SUBSTANCE-BROADENING claim (functional continuity of pre-1923 religious governance) — measuring it would show high-value unremedied claims (seminary, foundations, self-administration) against the host state. The upstream/downstream structure runs from this reading outward: the adjudication pathway this reading posits is the vehicle through which expansive claims travel and the target at which restrictive denial aims, so this story links to both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
