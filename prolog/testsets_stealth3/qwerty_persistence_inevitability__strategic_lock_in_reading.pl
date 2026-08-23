% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Reading)
 *   domain: technology_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the strategic_lock_in_reading of the contested
 *   kernel qwerty_persistence_inevitability: the claim that QWERTY persists
 *   not by accident but because manufacturers engineered its persistence —
 *   through Remington's patent position and training partnerships, the 1893
 *   Union Typewriter consolidation that cartel-standardized the layout, and
 *   the sustained refusal to retool for alternative layouts that culminated
 *   in Dvorak's exclusion. On this reading the standing arrangement possesses
 *   BOTH a genuine coordination function (universal skill transfer, hardware
 *   interchange, a liquid labor pool) AND asymmetric extraction (ergonomic
 *   costs and retraining barriers borne by typists who never chose, foregone
 *   alternatives, and historical standard-control rents). The sibling
 *   reading, path_dependency_reading, is a separate constraint story linked
 *   via network.affects_constraints: the colloquial label 'QWERTY
 *   persistence' decomposes under the epsilon-invariance principle into two
 *   structurally distinct claims with different beneficiary/victim sets and
 *   different epsilon. Claim and metrics are authored independently:
 *   claimed_type states this reading's structural thesis; the metric values
 *   state what the historical and current record descriptively shows under
 *   this reading's lights, including the end-state softening the reading
 *   itself must acknowledge.
 *
 * KEY AGENTS:
 *   - - remington_typewriter_company: agenda setter (institutional/arbitrage) — held the patents, set the shipping layout, led the 1893 consolidation
 *   - - union_typewriter_trust_members: cartel beneficiaries (powerful/constrained) — signed the standardization agreement, shared the trained-labor pool
 *   - - qwerty_training_schools: pipeline beneficiaries (organized/constrained) — the exclusive instruction channel that manufactures QWERTY-only typists
 *   - - keyboard_hardware_manufacturers: inherited-standard beneficiaries (institutional/mobile) — ship the default at zero establishment cost
 *   - - qwerty_typists: primary targets (powerless/constrained) — bear ergonomic costs and retraining barriers, never consulted
 *   - - alternative_layout_inventors: excluded targets (moderate/trapped) — Dvorak et al., market access gated by the standard's control points
 *   - - office_employers: dual-positioned (organized/mobile) — demand QWERTY-trained labor, bear equipment standardization
 *   - - keyboard_standards_bodies: administrators (institutional/constrained) — codify and reaffirm, collect nothing
 *   - - ergonomics_researchers: analytical observers — the contested efficiency evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.56).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.36).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'fb2c3e7f-d213-491c-ad8c-846394a4df34').
narrative_ontology:cs_kernel_codification('fb2c3e7f-d213-491c-ad8c-846394a4df34', formalized).
narrative_ontology:cs_authority_grounding('fb2c3e7f-d213-491c-ad8c-846394a4df34', extraction).
narrative_ontology:cs_interpretation_layer_present('fb2c3e7f-d213-491c-ad8c-846394a4df34').
narrative_ontology:cs_reading_relation('fb2c3e7f-d213-491c-ad8c-846394a4df34', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('fb2c3e7f-d213-491c-ad8c-846394a4df34', foundational, persistence_is_engineered_outcome).
narrative_ontology:cs_axiom_status(persistence_is_engineered_outcome, holdable).
narrative_ontology:cs_axiom_grounding('fb2c3e7f-d213-491c-ad8c-846394a4df34', persistence_is_engineered_outcome, empirically_contingent).
narrative_ontology:cs_axiom('fb2c3e7f-d213-491c-ad8c-846394a4df34', secondary, standard_control_yields_extractive_leverage).
narrative_ontology:cs_axiom_status(standard_control_yields_extractive_leverage, holdable).
narrative_ontology:cs_axiom_grounding('fb2c3e7f-d213-491c-ad8c-846394a4df34', standard_control_yields_extractive_leverage, instrumental).
narrative_ontology:cs_reference_frame('fb2c3e7f-d213-491c-ad8c-846394a4df34', cartel_administered_standard_regime).
narrative_ontology:cs_drift_state('fb2c3e7f-d213-491c-ad8c-846394a4df34', contemporary_post_cartel_inertia, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb2c3e7f-d213-491c-ad8c-846394a4df34', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_company).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_training_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, office_employers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, office_employers).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, manufacturer_standard_control_strategy).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, training_pipeline_gatekeeping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held the Sholes patents, arranged manufacture and sale of the first commercially successful typewriters, and decided which keyboard layout its machines shipped with. Built the sales-and-service network and the earliest typing-instruction partnerships, supplying machines to schools that agreed to teach its layout. Led the 1893 consolidation under which rival makers agreed to standardize on the same keyboard. The company later diversified beyond typewriters; the layout decision it took in the 1870s-90s outlived its typewriter business entirely.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_company, agenda_setter,
    institutional, generational, arbitrage, continental).

% The typewriter manufacturers — Yost, Densmore, Smith Premier, and allied firms — that joined the 1893 consolidation and agreed to build QWERTY machines exclusively, sharing the growing pool of QWERTY-trained operators and stabilizing prices across the trade. Each firm kept its own brand and sales force while honoring the common layout. Their agreement held for roughly two decades before antitrust pressure and renewed competition frayed it; the firms themselves are long defunct.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members, agenda_setter).

% Commercial typing academies and, later, business colleges and public vocational programs that taught touch typing. Manufacturers supplied machines free or at cost and steered graduates toward employers demanding QWERTY operators. Curriculum, certification, and graduate placement all presuppose the single layout: teaching any other layout would strand graduates in a labor market that asks for QWERTY, so the schools' product is only as portable as the standard is universal.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_training_schools, beneficiary,
    organized, biographical, constrained, global).

% Typewriter, terminal, and personal-computer keyboard makers who ship QWERTY as the factory default. None of them paid the nineteenth-century costs of establishing the layout, yet each inherits a customer base that already knows it and would treat any other default as a defect. Offering an alternative layout as default invites returns and support burden; shipping QWERTY costs nothing and matches every buyer's expectation. Any one of them could deviate; none has an incentive to move first.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_hardware_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Everyone who learned to type on the standard layout — office workers, programmers, students. They were never offered a choice of layout at the point of learning; the ergonomic profile they live with (longer finger travel, a staggered row geometry that ergonomic critics fault) was fixed by decisions taken before they were born. Adopting an alternative layout means weeks of lost speed, relabeled or blank keycaps, and losing fluency on every shared machine at work, school, and home. As a class they have never been surveyed or consulted on layout choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_typists, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_typists, excluded).

% Designers of competing layouts — August Dvorak and William Dealey most prominently, later the Colemak author — who patented or published arrangements claiming measurable improvements and sought manufacturer adoption, school adoption, or standards ratification. Dvorak secured a government study and wartime trials, but no major manufacturer would retool and no school system switched; his layout survives as an option buried in operating-system settings. Their route to market runs through the same manufacturers, schools, and standards bodies that the incumbent arrangement's beneficiaries populate.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, payer,
    moderate, biographical, trapped, global).

% Firms that hire typists and buy equipment. They gain from a labor pool whose skills transfer between any two employees and any two machines, and they would lose whenever a layout change idled their workforce during retraining. Their hiring requirements ('QWERTY proficiency') reproduce the standard each generation without anyone deciding it anew, and they bear the equipment-standardization side of the bargain: whatever the layout, they must buy what their workers can type on.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, office_employers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, office_employers, payer).

% National and international standards committees (ANSI, ISO) that codified the layout in formal standards documents beginning in the late 1960s and reaffirm it in periodic revisions. Codification is their function; they collect no revenue tied to which layout wins, and revising the codified layout would require consensus among member manufacturers and governments that has never come close to forming. They administer a decision made elsewhere a century earlier.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Academic and government researchers who measured typing efficiency across layouts — the Dvorak-Dealey studies, the 1944 Navy experiment, and the later replications and critiques. They publish findings that both sides of the layout dispute cite, and they hold no stake in which arrangement ships. Their measurements are the evidentiary battleground on which the ergonomic-cost claim rises or falls.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real bootstrapping problem: a single shared layout lets typing skill transfer across every machine, employer, and country, and gave early vendors a common standard around which to grow a trained-operator labor pool and interchangeable-equipment expectations. Any layout that had won would have provided this; the arrangement's specific content is simply the layout that won.
% TRANSFER_FUNCTION: Moves standard-control rents and avoided-transition benefits to manufacturers, training institutions, and the installed base; moves ergonomic costs, foregone alternative layouts, and retraining burdens onto typists and layout innovators. Concretely: machine sales and school placements flowed to cartel members; tuition flowed to schools; the costs of a suboptimal-but-universal layout settle diffusely on everyone who types.
% ABSENT_VOICES: Alternative-layout advocates had voice but no vote: Dvorak published, testified, and ran trials, yet the adoption decisions sat with manufacturer product committees, school boards, and procurement offices where he held no seat. Rank-and-file typists — the population bearing the ergonomic profile — were never consulted at any point in the standard's history. Non-Latin-script markets adapted QWERTY-derived arrangements (German QWERTZ, French AZERTY) through local committees that inherited rather than chose the base arrangement.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard and its supporting machinery vanished overnight, every keyboard, operating system, school curriculum, and hiring requirement would be orphaned simultaneously: hundreds of millions of fluent typists could not enter text on any device until retrained or until keycaps and defaults were re-engraved and re-shipped. Commerce, code, and administration would stall for months. The world rearranges around this arrangement completely — which is precisely why removing it now costs more than any plausible benefit.
% FOUNDING_PROBLEM: Two problems, per this reading: the mechanical one — adjacent typebars jamming when successive keystrokes struck nearby points on the platen — and the commercial one — selling typewriters required a supply of trained operators and a layout common across vendors so employers could hire interchangeable typists. The reading holds that the commercial problem, not the mechanical one, drove the layout's final form and its enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Technical historians of the typewriter mechanism (trade-press and museum literature on typebar interference) corroborate that jamming was a real 1870s engineering problem; economic historians (David 1985; Liebowitz and Margolis 1990 — writing entirely outside any beneficiary set, the beneficiary firms being defunct) corroborate that the trained-operator bootstrapping problem was solved by the 1920s. No living party attests that the founding problem is still live; the attestation that it is dead comes wholly from outside the benefiting parties.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.56: extraction is real but bounded — the per-typist ergonomic cost is modest and empirically contested (see dvorak_superiority_contest), so the bulk of measured extraction is foregone-alternative option value and the historical standard-control rents, not acute harm. Suppression 0.36 (end state): the enforcement machinery that peaked around the Dvorak exclusion has decayed; alternatives are legal and technically available, gated by ecosystem friction rather than force — suppression here is structural (pipeline gating, default placement, hiring requirements), not internalized, so no structural-versus-internalized omega is required. Theater_ratio 0.32: maintenance is increasingly performative (standards reaffirmations, ergonomic reviews that never revise) while the training pipeline remains functionally load-bearing. Accessibility_collapse 0.45: alternatives exist (OS-level Dvorak since 1984, Colemak since 2006) but collapse practically under retraining cost and shared-device incompatibility. Resistance 0.35: persistent niche advocacy, the ergonomic-keyboard movement, periodic research challenges — real but never organized at scale. The typist class illustrates the coalition failure the framework watches for in powerless victim sets: each typist's switch is private and costly, and the class action (simultaneous retraining of everyone) is exactly what the standard's universality makes irrational for any individual — the collective-action problem is the lock-in's load-bearing wall. Measurement series run on one shared seven-point grid (1873, 1893, 1936, 1961, 1984, 2006, 2026) with every tracked metric authored at every point; end-state values match base_properties by construction. Assumptions stated: the interval anchors to the 1873 Remington-Sholes commercial introduction and the present; metric values are authored judgments from the historical record under this reading's lights, not instrument readings.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement reads as a choice made once, elsewhere, forever: typists inherit an ergonomic profile they never selected and cannot cheaply leave; layout inventors watch adoption decisions close from outside the rooms where they are made. From the beneficiary seats the same structure reads as an achievement: manufacturers see a stable platform they built or prudently joined, schools see a teachable certifiable skill, employers see liquid labor. The sharpest same-power divergence is institutional-versus-institutional: keyboard manufacturers (mobile exit) experience the standard as a free inheritance they could abandon at will, while standards bodies (constrained exit) experience it as a codification they must defend through consensus they cannot command. Historically, Remington (arbitrage exit) could steer the standard while trust members (constrained by the agreement) could only follow it — nominally equal cartel powers, opposite experienced constraint sets. Inter-institutionally, schools and employers are the transmission belt: neither set the layout, but both reproduce it every hiring cycle and every enrollment.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map to directionality as follows. Beneficiaries sit at the low-d end: Remington and the trust members controlled the standard and collected its rents (the trust members' constrained exit reflects binding cartel commitments); training schools monetize the single-curriculum pipeline; hardware manufacturers inherit the default at zero cost with mobile exit, placing them near-beneficiary but not at zero, since they bear mild first-mover risk against the installed base. Victims sit at the high-d end: typists bear ergonomic and retraining costs with constrained (not trapped) exit — software layouts exist — so their d sits just short of full target; layout inventors are trapped, their entire project gated by the standard's control points, placing them at the full-target end. Office employers are genuinely dual-positioned and derive near-symmetric. Standards bodies administer without collecting; with no beneficiary/victim declaration their d falls to the power-atom fallback, which I accept rather than override — the symmetric-administrator reading is structurally accurate. No directionality_overrides are authored: the derivation chain produces the right relationships from the declared structural data, and the override mechanism keys on power atoms, which would be too blunt to differentiate the same-power seats this story distinguishes by exit options. Per the framework's division of labor, the suppression value above is a raw structural property left unscaled; the engine alone scales extractiveness by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems are dead: no typebars have jammed since electromechanical designs displaced strikers, and the trained-operator pool bootstrapped itself a century ago. Yet the arrangement persists and the world would rearrange violently without it — the status-dead x world-rearranges mismatch flags zombie/capture risk and routes scrutiny to the computed theater and receipt paths. The receipt surface sharpens the question: gains that were once concentrated (Remington, then the trust) are now diffuse across the installed base, and fixing is prohibitive — a cell that pattern-matches to degraded-inertial persistence rather than live capture. This reading nonetheless claims the hybrid structure, on the strength of still-active mechanisms: curricula that teach only QWERTY, defaults that ship only QWERTY, hiring requirements that screen for it. Whether those mechanisms constitute live enforcement or administered inertia is precisely the open question routed to enforcement_liveness_endstate rather than resolved by assertion. Mandatrophy discipline prevents the two symmetrical mislabelings: reading the cartel era backward would paint the present as pure extraction with a living capturer (no such seat exists — hence gain_flow 'diffuse', authored affirmatively after checking every named seat: the defunct firms collect nothing, school tuition is standardization-generic rather than layout-specific, hardware margins are competitive, and the largest benefit accrues to everyone who already knows the layout); ignoring the historical engineering would dissolve the arrangement into innocent accident and erase the victims this reading holds real. The classification keeps both strands visible and lets the temporal series — enforcement rising, peaking, decaying while extraction plateaus and theater creeps up — carry the lifecycle verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the strategic_lock_in_reading of kernel qwerty_persistence_inevitability; the sibling reading path_dependency_reading holds that QWERTY persisted through accident-driven network effects with no strategic beneficiaries. Which causal structure does the archival record actually support?',
    'Archival scoring of deliberate-coordination evidence: Union Typewriter 1893 agreement terms and minutes, Remington training-school contracts and machine-placement records, manufacturer licensing refusals to alternative-layout applicants — coded for intentional standard control versus emergent response to externalities already in motion.',
    'If the path_dependency_reading dominates, this story''s beneficiary and victim sets dissolve, epsilon collapses toward the coordination floor, and the classification shifts toward an inertial or pure-coordination account. If the strategic reading dominates, the authored structure stands as written.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Committer-frame routing of the kernel contest between engineered lock-in and accidental path dependency.').

omega_variable(
    dvorak_superiority_contest,
    'Does the victim set''s ergonomic-cost component rest on real efficiency losses (Dvorak-Dealey studies, the 1944 Navy experiment) or on overstated claims (the Liebowitz-Margolis critique of the fable of the keys)?',
    'Independent replication of layout-efficiency comparisons with modern instrumentation and controls for learner motivation and experimenter allegiance.',
    'If the efficiency differential is negligible, the victim set shrinks to retraining-barrier bearers and excluded innovators, epsilon drops materially, and the extraction story narrows from bodily cost to foregone option value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_superiority_contest, empirical, 'Contested empirical foundation of the ergonomic victim set.').

omega_variable(
    cartel_intentionality_attribution,
    'Was the 1893 standardization agreement deliberate lock-in strategy, or ordinary industry consolidation riding network externalities that were already forming regardless?',
    'Minute books, internal correspondence, and contract clauses of the Union Typewriter consolidation, read against the counterfactual timing of training-network growth.',
    'Determines whether the engineered component of this reading is load-bearing (supporting the claimed hybrid structure) or decorative (collapsing this story toward the sibling''s account with a historical costume).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intentionality_attribution, empirical, 'Attribution of intentionality in the 1893 cartel standardization.').

omega_variable(
    enforcement_liveness_endstate,
    'Do the training pipeline and default-placement practices still constitute active enforcement of the standard today, or has the arrangement crossed into administered inertia where nothing is defended and nothing would change if defenders stopped?',
    'Observe whether any actor currently incurs costs defending the standard against alternatives — curriculum-retention decisions, default-firmware choices, procurement specifications — as distinct from the mere absence of change.',
    'If enforcement is live, the claimed hybrid structure stands for the end state; if the arrangement is inertial, the end state reclassifies toward the degraded-inertial type, consistent with the diffuse-gains and prohibitive-fix receipt data authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_liveness_endstate, conceptual, 'Whether end-state maintenance is active engineering or administered inertia (framing-dependent).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lockin_reading_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t1873, observed).
narrative_ontology:measurement(qwerty_lockin_reading_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.11).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t1893, observed).
narrative_ontology:measurement(qwerty_lockin_reading_tr_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1936, 0.2).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t1936, observed).
narrative_ontology:measurement(qwerty_lockin_reading_tr_t1961, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1961, 0.24).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t1961, observed).
narrative_ontology:measurement(qwerty_lockin_reading_tr_t1984, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1984, 0.28).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t1984, observed).
narrative_ontology:measurement(qwerty_lockin_reading_tr_t2006, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2006, 0.3).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t2006, observed).
narrative_ontology:measurement(qwerty_lockin_reading_tr_t2026, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2026, 0.32).
narrative_ontology:measurement_basis(qwerty_lockin_reading_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(qwerty_lockin_reading_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.16).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t1873, observed).
narrative_ontology:measurement(qwerty_lockin_reading_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.33).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t1893, observed).
narrative_ontology:measurement(qwerty_lockin_reading_be_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1936, 0.54).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t1936, observed).
narrative_ontology:measurement(qwerty_lockin_reading_be_t1961, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1961, 0.61).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t1961, observed).
narrative_ontology:measurement(qwerty_lockin_reading_be_t1984, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1984, 0.64).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t1984, observed).
narrative_ontology:measurement(qwerty_lockin_reading_be_t2006, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2006, 0.59).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t2006, observed).
narrative_ontology:measurement(qwerty_lockin_reading_be_t2026, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2026, 0.56).
narrative_ontology:measurement_basis(qwerty_lockin_reading_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lockin_reading_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.14).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t1873, observed).
narrative_ontology:measurement(qwerty_lockin_reading_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.48).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t1893, observed).
narrative_ontology:measurement(qwerty_lockin_reading_su_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1936, 0.71).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t1936, observed).
narrative_ontology:measurement(qwerty_lockin_reading_su_t1961, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1961, 0.58).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t1961, observed).
narrative_ontology:measurement(qwerty_lockin_reading_su_t1984, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1984, 0.46).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t1984, observed).
narrative_ontology:measurement(qwerty_lockin_reading_su_t2006, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2006, 0.39).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t2006, observed).
narrative_ontology:measurement(qwerty_lockin_reading_su_t2026, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2026, 0.36).
narrative_ontology:measurement_basis(qwerty_lockin_reading_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'QWERTY persistence' decomposes under the epsilon-invariance principle into two structurally distinct constraint stories: this strategic_lock_in_reading (engineered lock-in via cartel standardization and training-pipeline control; identifiable beneficiaries and victims; epsilon 0.56) and the sibling path_dependency_reading (accident-driven network-effect persistence with no strategic maintenance; beneficiary/victim sets dissolve; epsilon near the coordination floor). The decomposition exists because measuring persistence 'as engineered' and 'as accidental' yields irreconcilable epsilon values for what the language treats as one claim. The sibling is the more established academic frame (David 1985) and functions upstream as the null hypothesis this reading argues against; each story links the other via network.affects_constraints and routes the contest to the kernel_reading_contest omega rather than averaging across readings inside either file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
