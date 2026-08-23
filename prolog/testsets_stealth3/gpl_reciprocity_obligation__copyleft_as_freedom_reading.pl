% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation — Copyleft as Freedom Reading
 *   domain: technological/legal
 *
 * SUMMARY:
 *   The GNU General Public License family conditions redistribution of
 *   covered code on passing the same freedoms downstream: derivative works
 *   must carry the same license, and source must accompany distribution. This
 *   story instantiates the FREEDOM READING of that arrangement: viral
 *   licensing preserves user freedoms by preventing proprietary capture. The
 *   epsilon referent is the standing arrangement under contest — the
 *   reciprocity obligation as it actually operates — assessed by this
 *   reading's own lights, in which the burden on proprietary integrators is
 *   the enforcement mechanism of a freedom guarantee rather than rent
 *   collection. The claim/metric gap is deliberate and load-bearing: the
 *   reading CLAIMS tangled_rope (real coordination function plus a defined
 *   class bearing asymmetric enforced costs) while the metrics are authored
 *   from the arrangement's observable operation — high suppression of
 *   alternative licensing paths, moderate and slowly accumulating extractive
 *   pressure, low theater. The engine computes per-seat classifications from
 *   the structural data; where its verdict diverges from this claim, that
 *   divergence is the measurement. Sibling readings of the same kernel are
 *   separate constraints (separate files), not hedges inside this one.
 *
 * KEY AGENTS:
 *   - - downstream_users: Primary beneficiary (moderate/constrained) — hold guaranteed freedoms across every redistribution chain; exit means leaving ecosystem gravity
 *   - - free_software_contributors: Beneficiary (moderate/identity_locked) — contribute under the guarantee that public work stays public; ideologically fused with the norm
 *   - - open_source_businesses: Beneficiary (powerful/constrained) — competitive moat depends on the shared stack being unclosable; absorbs compliance costs as the price
 *   - - dual_license_vendors: Beneficiary and capture seat (powerful/mobile) — revenue scales with demand for relief from the obligation they publicly defend
 *   - - proprietary_integrators: Primary target/payer (powerful/constrained) — bear foreclosure of closed-derivative options; strongest where components are irreplaceable
 *   - - free_software_foundations: Agenda setter (institutional/identity_locked) — publish, interpret, revise, and enforce the license; institutionally fused with its function
 *   - - embedded_device_owners: Nominal beneficiary, practically excluded (powerless/trapped) — hold rights that signed bootloaders make unexercisable
 *   - - ip_law_scholars: Analytical observer (institutional/analytical) — analyze enforceability and doctrinal novelty without taking adoption sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.48).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.73).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation — Copyleft as Freedom Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "technological/legal").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '371cc327-13a2-422a-9c52-5f428ffbe504').
narrative_ontology:cs_kernel_codification('371cc327-13a2-422a-9c52-5f428ffbe504', fixed_text).
narrative_ontology:cs_authority_grounding('371cc327-13a2-422a-9c52-5f428ffbe504', lineage).
narrative_ontology:cs_interpretation_layer_present('371cc327-13a2-422a-9c52-5f428ffbe504').
narrative_ontology:cs_reading_relation('371cc327-13a2-422a-9c52-5f428ffbe504', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('371cc327-13a2-422a-9c52-5f428ffbe504', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('371cc327-13a2-422a-9c52-5f428ffbe504', foundational, user_freedom_trumps_integration_liberty).
narrative_ontology:cs_axiom_status(user_freedom_trumps_integration_liberty, holdable).
narrative_ontology:cs_axiom_grounding('371cc327-13a2-422a-9c52-5f428ffbe504', user_freedom_trumps_integration_liberty, deontological).
narrative_ontology:cs_axiom('371cc327-13a2-422a-9c52-5f428ffbe504', foundational, reciprocity_is_constitutive_of_durable_freedom).
narrative_ontology:cs_axiom_status(reciprocity_is_constitutive_of_durable_freedom, holdable).
narrative_ontology:cs_axiom_grounding('371cc327-13a2-422a-9c52-5f428ffbe504', reciprocity_is_constitutive_of_durable_freedom, instrumental).
narrative_ontology:cs_reference_frame('371cc327-13a2-422a-9c52-5f428ffbe504', four_freedom_guarantee_baseline).
narrative_ontology:cs_drift_state('371cc327-13a2-422a-9c52-5f428ffbe504', contemporary_cloud_delivery_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('371cc327-13a2-422a-9c52-5f428ffbe504', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_contributors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_businesses).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, dual_license_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, embedded_device_owners).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, four_software_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run, study, modify, and redistribute software under the license's grants. They receive source access and modification rights that persist through every redistribution chain. Switching to non-reciprocal alternatives means leaving ecosystems — kernels, compilers, core utilities — whose installed base and tooling they depend on daily.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, constrained, global).

% Write code into projects whose license bars later proprietarization of their contributions. Their work accumulates in a pool no competitor can close. Taking proprietary employment is a normal career move, but contributing published work under other terms would mean abandoning the guarantee that what they give publicly stays public.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_contributors, beneficiary,
    moderate, biographical, identity_locked, global).

% Build products and services on reciprocally licensed stacks and sell support, hosting, and hardware around them. They invest heavily in compliance engineering and contribute upstream. Their competitive position depends on rivals being unable to close forks of the shared stack; absorbing the license's obligations is the price of that moat.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_businesses, beneficiary,
    powerful, generational, constrained, global).

% Publish their code under reciprocal terms while selling paid exceptions to the same terms. Revenue scales with how much customers want relief from the reciprocal obligation. They defend the license's strictness in public governance debates and monetize exemptions from it in private contracts.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, dual_license_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Want to combine reciprocally licensed components with closed products. The license requires releasing derivative source under the same terms, which their business models treat as unacceptable. Their options: avoid the components entirely, fund clean-room rewrites, purchase separate commercial terms where a vendor offers them, or face enforcement after infringement findings. Where the component is irreplaceable — a kernel, a compiler — avoidance is not realistic.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Hold copyrights, publish and maintain the license texts, run compliance programs, and pursue enforcement against violators. Donation-funded; staffed by people whose careers and self-conception are bound to the mission. They decide when the license text is revised and which violations get pursued.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_foundations, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Own hardware shipping reciprocally licensed firmware. On paper they hold the same source-access and modification rights as any user; in practice, cryptographic boot signatures and withheld build environments make exercise impossible for nearly all of them. They have no seat in license drafting, revision, or enforcement prioritization.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, embedded_device_owners, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, embedded_device_owners, excluded).

% Analyze the license as a legal instrument: its enforceability across jurisdictions, its inversion of copyright's usual direction (using exclusive rights to compel disclosure rather than prevent it), and its interaction with contract and patent law. They publish, advise courts and legislatures, and take no side in adoption decisions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, ip_law_scholars, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, dual_license_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the appropriation problem in distributed software production: without a reciprocity term, any participant could take the pooled code, extend it privately, and release the extension as a closed product, draining improvements from the common pool. The same-license requirement makes continued sharing the standing condition of building on others' work.
% TRANSFER_FUNCTION: Moves source-code access and modification rights from would-be private appropriators to the public: integrators surrender the option of closed derivatives; downstream users receive guaranteed freedoms that survive every redistribution.
% ABSENT_VOICES: Proprietary integrators appear only as adversaries in enforcement dockets, never as co-drafters of the norm they are bound by. Owners of embedded devices affected by locked bootloaders had no representation until GPLv3, and none with voting weight even then. Permissive-license advocates exited the conversation rather than argue within it.
% DISAPPEARANCE_RATIONALE: If the reciprocity term vanished overnight, closed forks of the major reciprocally licensed infrastructure — distributions, compilers, core utilities — would appear within product cycles; contributor expectations and the gift-economy norms built on guaranteed publicity would unwind; dual-license revenue models would lose their basis; the software commons would reorganize around whatever replacement norm emerged or fragment along permissive lines.
% FOUNDING_PROBLEM: In the late 1970s and early 1980s, freely shared programs were routinely taken proprietary: vendors accepted community improvements, closed the result, and stopped publishing source — the canonical case being a printer driver whose source was withheld from the person asked to fix it. The arrangement was built to make such appropriation structurally impossible: to keep software studyable, modifiable, and shareable regardless of who improves it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: business historians of the Unix wars document the proprietary-fragmentation wave the license answered; intellectual-property counsel at integrator firms treat compliance as genuine legal exposure, attesting that the obligation binds; adversarial industry strategy documents from the 1990s acknowledged the appropriation dynamic the license targets. No corroborating source outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48: the arrangement imposes real, asymmetric, legally enforced costs on a defined class (closed-derivative options of integrators), but from this reading's lights those costs are the guarantee's price, not collected rent — hence moderate, not high. Suppression is high (0.73) per the expected structural delta: the license terms plus active enforcement foreclose proprietary-integration alternatives for covered code, and the reading itself endorses this foreclosure. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater is low (0.22): compliance ceremonies exist (license-header rituals, written-offer letters, compliance paperwork) but the core function — freedoms surviving redistribution — demonstrably operates. Accessibility_collapse (0.62) reflects that alternatives collapse almost completely for code already under the license (relicensing out is famously hard) while the wider ecosystem retains permissive alternatives. Resistance (0.58) reflects decades of sustained corporate opposition: the 1990s 'viral license' campaigns, the GPLv3/TiVoization fight, permissive-stack migration. The temporal series run on ONE shared grid (t=0,6,12,18,24,30,36 over 1989–2025) with all three metrics authored at every point; the rising suppression_requirement series traces deliberate enforcement-capacity buildup (compliance labs, gpl-violations.org, the BusyBox suits, the VMware action) — functional intensification, not decay — and the gently rising base_extractiveness tracks that buildup landing on the target class.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the proprietary_integrator seat (payer, powerful, constrained exit, high directionality), the arrangement presents as enforced foreclosure of ordinary business form — the restriction reading's whole case lives in this seat's experience. From the downstream_user and contributor seats (beneficiaries, low directionality), the same structure presents as a subsidy of freedom — the rope-like face. From the foundation seat (agenda_setter, identity_locked), the arrangement is indistinguishable from the institution's own mission. The dual_license_vendor seat is the sharpest anomaly: declared beneficiary, yet its revenue is a positive function of the obligation's bite — the derivation assigns it low directionality from its role, and the residual tension is documented in the kernel_context note and the receipt surface rather than papered over with an override.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (users, contributors, businesses, dual-license vendors, device owners) drive low directionality for those seats; the victim declaration (proprietary_integrators) drives high directionality, amplified by constrained exit — their alternatives (component avoidance, clean-room rewrites) are real but expensive and unavailable precisely where the covered component is irreplaceable. No directionality_overrides are authored: the override mechanism keys on power_atom, and this story's power atoms are shared by agents with opposed structural relationships (the 'powerful' atom alone contains integrators, businesses, and dual-license vendors), so any per-atom override would smear across seats the derivation already separates correctly by role and exit. Scope is global for most seats, which modestly amplifies effective extraction on the target seat through verification difficulty — enforcement across jurisdictions is exactly what the compliance-lab buildup addresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — structural impossibility of appropriating shared code — remains live and externally corroborated, so the arrangement is not a mandate outliving its function. The mismatch guard agrees: founding_problem_status=live crossed with disappearance_verdict=world_rearranges produces no zombie flag. The classification resists both standard mislabels: calling this pure coordination ignores that a defined class bears enforced asymmetric costs through the same structure (hence not rope); calling it pure extraction ignores that the coordination function is real, primary, and operating (hence not snare) — the theater ratio stays low and the enforcement buildup tracks genuine appropriation attempts, not vestigial ritual. The live risk to the mandate is the SaaS blind spot (omega saas_loophole_currency), not atrophy of enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (copyleft_as_freedom_reading) of the kernel gpl_reciprocity_obligation; the sibling readings copyleft_as_restriction_reading and copyleft_as_commons_reading instantiate different constraints from the same license text. Which reading correctly locates the beneficiary/victim structure, and where exactly do the readings disagree?',
    'Cross-reading comparison of the three compiled sibling stories: computed per-seat classifications and epsilon values under each reading. Divergence localizes the disagreement to whether the burden on proprietary integrators counts as extraction (restriction reading), as constitutive boundary maintenance (freedom reading), or as the cost of enclosure-prevention (commons reading).',
    'Adopting the restriction reading raises epsilon and widens the victim set toward all commercial integrators; adopting the commons reading shifts the beneficiary locus to the commons as such and downweights individual users. This story''s tangled_rope claim holds only under the freedom reading''s valuation of the same structural facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the GPL reciprocity kernel fixes the true beneficiary/victim structure.').

omega_variable(
    suppression_endorsement_status,
    'This reading reports high suppression (0.73) of alternative licensing paths while endorsing that suppression as the very mechanism of the freedom guarantee. Is the measured suppressive force coercive overhead, or constitutive boundary maintenance that the reading itself calls for?',
    'Compare the authored suppression series against the sibling stories'' authored suppression and the engine''s per-seat chi outputs: if the payer seat computes a snare-flavored verdict while beneficiary seats compute coordination, the endorsement stance is what carries the classification.',
    'If suppression is constitutive, estimated excess harm drops and the coordination component dominates the verdict; if coercive, the arrangement trends toward the restriction reading''s more extractive verdict despite this reading''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_endorsement_status, conceptual, 'Whether the constraint''s high suppression is endorsed boundary maintenance or coercive overhead.').

omega_variable(
    freedom_exercise_gap,
    'Do downstream users actually exercise the granted freedoms — rebuilding firmware, modifying, redistributing — or are the freedoms nominal for most recipients, especially embedded-device owners facing signed bootloaders and withheld build tools?',
    'Empirical audit: fraction of recipients who rebuild or modify shipped software, firmware-unlock rates by device class, and interaction patterns (hosted services) where source access yields no exercisable freedom.',
    'If exercise is rare, the beneficiary declaration overstates the served population and the arrangement increasingly protects contributor-class and business interests; beneficiary-side directionalities rise toward symmetric and the freedom framing loses empirical footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_exercise_gap, empirical, 'Whether the freedoms the constraint guarantees are exercised or merely nominal.').

omega_variable(
    saas_loophole_currency,
    'The founding problem targeted binary-only appropriation; network-delivered services can absorb community improvements without ever distributing binaries the license reaches. Is the founding problem still live under cloud delivery, or has the constraint''s structural blind spot hollowed it?',
    'Track AGPL-family adoption rates, the ratio of service-only deployments to source-available deployments for major reciprocally licensed projects, and enforcement actions against service-only appropriation.',
    'If the loophole dominates, the founding problem flips toward dead/contested and the arrangement drifts toward theatrical maintenance of a guarantee that no longer binds the main appropriation path; if network-copyleft patching spreads, the problem remains live and enforcement stays functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_loophole_currency, empirical, 'Whether cloud delivery has made the founding problem obsolete for the license''s main corpus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t6, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement_basis(gpl__tr_t18, observed).
narrative_ontology:measurement(gpl__tr_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t24, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t36, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement_basis(gpl__be_t6, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement_basis(gpl__be_t18, observed).
narrative_ontology:measurement(gpl__be_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement_basis(gpl__be_t24, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t36, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 36, 0.48).
narrative_ontology:measurement_basis(gpl__be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(gpl__su_t6, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(gpl__su_t18, observed).
narrative_ontology:measurement(gpl__su_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(gpl__su_t24, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t36, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 36, 0.73).
narrative_ontology:measurement_basis(gpl__su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'GPL viral licensing' decomposes into three structurally distinct constraints sharing one kernel (gpl_reciprocity_obligation). This story instantiates the freedom reading (epsilon 0.48; beneficiary = downstream users; victim = proprietary integrators; suppression endorsed as constitutive). The restriction reading authors higher epsilon with commercial integrators broadly as victims; the commons reading locates the beneficiary in the commons as such and treats the reciprocity term as anti-enclosure institutional technology. Same license text, different epsilon, different victim sets, different classifications — modeled as three linked stories, not one story with a measurement parameter. Upstream/downstream: the freedom reading is the historically prior rhetorical frame and supplies legitimacy conditions the commons reading operates within (influences edge); the restriction reading competes as a live adversarial position (coexists_with edge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
