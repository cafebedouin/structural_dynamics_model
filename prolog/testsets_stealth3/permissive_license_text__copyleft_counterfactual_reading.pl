% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Non-Reciprocal Copyright Relaxation (Copyleft Counterfactual Reading)
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the permissive license text itself (MIT/BSD/Apache-class instruments) as
 *   the dominant mode of copyright relaxation in software. Three readings of
 *   that text coexist as separate constraint stories:
 *   commons_coordination_reading (relaxation as friction-free universal
 *   freedom, near-zero extraction), corporate_moat_reading (relaxation as
 *   deliberate enclosure strategy feeding proprietary derivatives), and THIS
 *   story, copyleft_counterfactual_reading: the copyleft movement's
 *   counterfactual claim that relaxation WITHOUT a reciprocity requirement
 *   structurally channels commons labor into closed products, and that viral
 *   share-alike licensing is the necessary corrective. Per the
 *   epsilon-referent rule, this story authors epsilon for the STANDING
 *   arrangement (the non-reciprocal relaxation landscape as it operates) as
 *   this reading sees it — high, because the reading reads the arrangement as
 *   systematic uncompensated appropriation — never for the GPL regime the
 *   reading would install. KEY AGENTS (by structural relationship): -
 *   proprietary_software_vendors: Primary beneficiary
 *   (institutional/arbitrage) — collects closed-product revenue on commons
 *   inputs - volunteer_open_source_contributors: Primary target
 *   (moderate/identity_locked) — supplies uncompensated labor with no
 *   contractual return - copyleft_project_maintainers: Dual-positioned
 *   target-and-recipient (organized/identity_locked) — bear competitive
 *   displacement while consuming the same frictionless commons -
 *   corporate_platform_contributors: Secondary recipient-payer
 *   (institutional/mobile) — pay sponsored engineering, harvest strategic
 *   control - license_policy_foundations: Agenda setter
 *   (institutional/identity_locked) - permissive_project_maintainers:
 *   Project-level agenda setters (moderate/constrained) - software_end_users:
 *   Diffuse target (powerless/constrained) - deterred_commons_contributors:
 *   Excluded voice (powerless/mobile) - licensing_ip_analysts: Analytical
 *   observer. The claimed type and the metrics are independent authored
 *   facts: this reading claims tangled_rope because it affirms a real
 *   coordination function even while asserting heavy asymmetric extraction;
 *   the metrics describe operation as the reading assesses it.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: primary beneficiary (institutional/arbitrage) — collects closed-product rents on permissive commons inputs
 *   - volunteer_open_source_contributors: primary target (moderate/identity_locked) — supplies uncompensated engineering labor
 *   - copyleft_project_maintainers: dual-positioned target-recipient (organized/identity_locked) — displaced competitors who nonetheless consume the commons
 *   - corporate_platform_contributors: secondary beneficiary-payer (institutional/mobile) — exchanges sponsored labor for ecosystem steering
 *   - license_policy_foundations: agenda setter (institutional/identity_locked)
 *   - permissive_project_maintainers: project-level agenda setter (moderate/constrained)
 *   - software_end_users: diffuse target (powerless/constrained) — receive derivatives stripped of the freedoms the commons assumed
 *   - deterred_commons_contributors: excluded voice (powerless/mobile) — never participate because capture is anticipated
 *   - licensing_ip_analysts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.76).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.48).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Non-Reciprocal Copyright Relaxation (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '4013de0a-3aa3-4022-be86-a07e9348202c').
narrative_ontology:cs_kernel_codification('4013de0a-3aa3-4022-be86-a07e9348202c', fixed_text).
narrative_ontology:cs_authority_grounding('4013de0a-3aa3-4022-be86-a07e9348202c', distributed).
narrative_ontology:cs_reading_relation('4013de0a-3aa3-4022-be86-a07e9348202c', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4013de0a-3aa3-4022-be86-a07e9348202c', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('4013de0a-3aa3-4022-be86-a07e9348202c', foundational, nonreciprocal_relaxation_structurally_exploitative).
narrative_ontology:cs_axiom_status(nonreciprocal_relaxation_structurally_exploitative, holdable).
narrative_ontology:cs_axiom_grounding('4013de0a-3aa3-4022-be86-a07e9348202c', nonreciprocal_relaxation_structurally_exploitative, empirically_contingent).
narrative_ontology:cs_axiom('4013de0a-3aa3-4022-be86-a07e9348202c', secondary, copyleft_virality_necessary_remedy).
narrative_ontology:cs_axiom_status(copyleft_virality_necessary_remedy, holdable).
narrative_ontology:cs_axiom_grounding('4013de0a-3aa3-4022-be86-a07e9348202c', copyleft_virality_necessary_remedy, instrumental).
narrative_ontology:cs_reference_frame('4013de0a-3aa3-4022-be86-a07e9348202c', reciprocal_sharing_baseline).
narrative_ontology:cs_drift_state('4013de0a-3aa3-4022-be86-a07e9348202c', contemporary_cloud_absorption_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4013de0a-3aa3-4022-be86-a07e9348202c', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, corporate_platform_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, volunteer_open_source_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, copyleft_project_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, software_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_project_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, corporate_platform_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incorporate permissively licensed code into closed and hosted products sold without source disclosure. They select intake projects through sponsorships and foundation funding, face no obligation beyond an attribution notice, and can fork, abandon, or internalize any component at will. Revenue from derivatives accrues to them; the underlying engineering labor was supplied elsewhere.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Supply code, review, documentation, and maintenance to permissively licensed projects without contractual compensation or a reciprocal claim on downstream users of their work. Their contributions persist in the commons indefinitely and cannot be recalled; stopping new contributions leaves past labor in circulation. Participation is bound up with their standing in the community and their sense of what skilled practitioners owe the shared infrastructure they learned from.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, volunteer_open_source_contributors, payer,
    moderate, biographical, identity_locked, global).

% Run share-alike licensed projects and argue publicly for reciprocity requirements. They compete for contributors and adoption against proprietary stacks built on frictionlessly reusable permissive inputs, and watch adjacent commons work close up inside commercial products. At the same time their own toolchains, build systems, and libraries lean heavily on permissive components they did not write, so the same low-friction arrangement they criticize is one they consume daily. Leaving the arena would mean abandoning projects that anchor their professional identity.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_project_maintainers, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, copyleft_project_maintainers, beneficiary).

% Large technology firms whose employees contribute paid engineering hours to permissive projects. They give back substantial labor and infrastructure, and in exchange gain outsized influence over project roadmaps, hiring pipelines, compatibility with their platforms, and early access to architectural direction. Their contributions are budgeted expenses recouped through strategic positioning; they can redirect effort across projects or bring work in-house at will.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, corporate_platform_contributors, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, corporate_platform_contributors, payer).

% Foundations and standards bodies that certify license definitions, host flagship projects, administer contributor agreements, and steer the normative boundary of acceptable licensing practice. Their charters, staff, and legitimacy are constituted around administering this arrangement; they cannot repudiate their own function without dissolving the organization. Funding flows disproportionately from corporate members who prefer non-reciprocal terms.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, license_policy_foundations, agenda_setter,
    institutional, generational, identity_locked, global).

% Individual maintainers who chose — or inherited — permissive texts for their projects and administer the resulting intake: merging external contributions, fielding commercial users, occasionally litigating attribution violations. Relicensing away from the chosen text requires tracking down and securing consent from every past contributor, which is practically prohibitive for mature projects, so the initial license decision binds them durably. Some are funded by the same commercial actors that consume their output.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, permissive_project_maintainers, agenda_setter,
    moderate, biographical, constrained, global).

% Receive software built on commons labor delivered as closed products or metered hosted services, without the inspection, modification, or redistribution rights the original grant carried. They had no seat in any license decision; their recourse is switching providers, which carries migration costs, retraining, and data-portability friction.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, software_end_users, payer,
    powerless, immediate, constrained, global).

% Skilled practitioners who decline to contribute to permissive projects at all because they anticipate their work being absorbed into closed products without return. They are outside the licensing conversation — no forum solicits their objection — and they express the withheld alternative simply by working elsewhere or contributing only to reciprocal projects.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, deterred_commons_contributors, excluded,
    powerless, biographical, mobile, global).

% Legal scholars, economists, and policy researchers who study license ecology, measure contribution flows, and publish analyses of reciprocity mechanisms. They hold no stake in any particular text and can see the whole seat structure at once; their work supplies the evidence base other seats argue with.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, licensing_ip_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common legal baseline under which any party may reuse, modify, merge, and redistribute code without prior negotiation, dissolving the per-integration contracting that an all-rights-reserved default imposes on multi-party software building.
% TRANSFER_FUNCTION: Moves engineering labor — code, review, documentation, maintenance — from commons contributors (volunteer and employer-sponsored) to whichever party incorporates the work, requiring attribution as the only return; in practice a large share lands in proprietary products sold without reciprocal release, and freedoms granted at the top of the chain do not reach end users of the derivatives.
% ABSENT_VOICES: Deterred potential contributors who never enter the ecosystem because anticipated uncompensated capture makes contribution irrational, and end users of closed derivatives who had no seat in any license selection. Both groups' objections exist only outside the venues where licensing norms are set; the conversation's apparent breadth conceals that the parties most affected by the no-reciprocity condition were never polled.
% DISAPPEARANCE_RATIONALE: If the permissive relaxation layer vanished overnight, vast portions of the software supply chain — cloud platforms, mobile stacks, machine-learning frameworks, embedded firmware — would lose their legal basis for combining components, forcing either wholesale renegotiation, mass relicensing under reciprocal terms, or frozen distribution. Pricing, product architectures, and corporate open-source strategies would all reorganize around whatever replaced the friction-dissolving function.
% FOUNDING_PROBLEM: Copyright's default all-rights-reserved posture made sharing code legally hazardous: every reuse, port, or integration required a bespoke agreement, strangling academic exchange and small-team collaboration in the early networked-software era. Permissive texts were drafted to dissolve that friction by pre-granting broad rights to everyone.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: intellectual-property legal histories of the Berkeley CSRG distributions and contemporaneous technical documentation corroborate that the negotiation-friction problem was real and general, and the copyleft movement itself — the sharpest critic of the no-reciprocity solution — corroborates that the founding problem existed while disputing that abandoning reciprocity was the necessary cure. No attestation rests solely on the proprietary vendors who collect from the current form.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.76 at interval end, rising from 0.52) because this reading measures the standing arrangement as a large, growing, one-way transfer of engineering labor into closed products with attribution as the only mandated return. Suppression is authored 0.48 — deliberately moderate, and a RAW structural value that the engine does not scale: nothing forbids a contributor or maintainer from choosing share-alike terms tomorrow, so coercion is far from total; what holds the arrangement together is sponsorship leverage, network dependency of downstream projects, and career signaling, plus an internalized openness ethos (see the exit-suppression omega for the structural/internalized split). Accessibility_collapse is low (0.25) because alternatives remain fully available — reciprocal licenses, dual licensing, relicensing — which is exactly what distinguishes this arrangement from a pure capture regime and supports the tangled_rope claim rather than a stronger extraction verdict. Resistance is substantial (0.6): the copyleft movement is itself the organized resistance, and recurring license controversies (restrictive-relicensing fights, definition-body rejections of pseudo-open terms) show continuous pushback. Theater_ratio is moderate-low (0.31): functional reuse dominates, but open-washing — marketing participation while shipping closed derivatives — is a growing performative layer. The suppression_requirement series is authored because the story specifically traces enforcement-capacity change: license-compliance machinery (software-composition analysis tooling, contributor-agreement audits, litigation capacity) matured steadily across the interval. All three series share one time grid (0..36 step 6) so every metric carries an authored value at every examined point; the end-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats on this arrangement should compute very different types from identical texts. From the vendor seat the arrangement is a subsidized coordination layer it did not have to build: near-full beneficiary directionality, costs trivially absorbed, everything downstream looks like voluntary exchange. From the volunteer-contributor seat the same texts operate as uncompensated requisition of skilled labor with identity-bound exit costs. The copyleft maintainer seat is genuinely split — consumer of the commons' convenience, competitor of its capturers — which is why the story declares that agent dual-positioned. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate between them, and the gap between the vendor seat's computed verdict and the contributor seats' computed verdicts is precisely the datum this reading exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. proprietary_software_vendors sit nearest the beneficiary pole: they collect the arrangement's gains, choose their intake freely, and hold arbitrage-grade exit. corporate_platform_contributors derive low directionality as declared beneficiaries — they pay sponsored labor but recoup more in steering, talent pipelines, and interoperability control. volunteer_open_source_contributors and software_end_users sit near the full-target pole: victims with constrained or identity-bound exits. copyleft_project_maintainers are declared victims yet are simultaneously heavy consumers of the frictionless commons; the derivation from the victim declaration alone would pin them at the target extreme, so a single directionality override at the organized power atom (d = 0.35) encodes their dual position — the only organized-power agent in the story, so the override touches exactly that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — copyright's default all-rights-reserved regime made any reuse legally fraught, requiring bespoke negotiation per integration — remains live: the texts still dissolve that friction daily, for beneficiaries and critics alike. Nothing here is atrophied or theatrically maintained; no sunset applies and none is claimed. The classification work this story performs is boundary-keeping in both directions: against mislabeling the arrangement as pure coordination (which erases the asymmetric transfer the beneficiary structure discloses) and against mislabeling it as pure capture (which erases a coordination function so real that its own loudest critics depend on it daily). Mandatrophy resolution is therefore not declared: the mandate and the extraction currently ride the same instrument, which is the defining condition the tangled_rope verdict records.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (copyleft_counterfactual_reading) of the kernel permissive_license_text, held alongside commons_coordination_reading and corporate_moat_reading — which reading''s beneficiary structure and epsilon should govern classification of the shared license text?',
    'Seat-level adjudication: the engine computes per-seat classifications from the declared beneficiary/victim structure of each reading-story; cross-reading comparison over the shared kernel reveals whether the disagreement tracks observable seat positions or only framing.',
    'Under commons_coordination_reading the same texts compute near-zero-extraction coordination; under corporate_moat_reading they compute as deliberate enclosure strategy. This reading''s high epsilon and tangled_rope verdict hold only while the copyleft counterfactual frame is the operative seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Reading-indexed status of the shared permissive-license kernel; disagreement located in beneficiary structure and extraction assessment.').

omega_variable(
    compensation_mix_ambiguity,
    'What fraction of contributions flowing into permissively licensed codebases is genuinely uncompensated volunteer labor versus employer-sponsored work that sponsors already recoup?',
    'Economic analysis linking commit authorship to employment status across major permissive ecosystems (kernel, JS/Python packaging, ML frameworks), measuring the truly-uncompensated share.',
    'If most contribution volume is employer-compensated, the extraction borne by volunteer contributors shrinks sharply and this reading''s epsilon falls toward the commons_coordination_reading''s; if volunteer labor remains a large share, the high extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_mix_ambiguity, empirical, 'Whether measured extraction targets real uncompensated labor or already-recouped sponsored work.').

omega_variable(
    virality_necessity_question,
    'Is viral share-alike licensing strictly NECESSARY to defend the code commons, as this reading''s foundational claim asserts, or are weaker instruments (network copyleft, patronage models, dual licensing, strategic relicensing) sufficient?',
    'Natural experiments from projects that added or removed reciprocity requirements (relicensing episodes, license-change controversies) and longitudinal comparison of commons sustainability under strong versus weak reciprocity regimes.',
    'If non-viral instruments prove sufficient, the reading''s foundational axiom loses its empirical footing and the prescription collapses into one option among several; if free-riding demonstrably depletes commons under weak reciprocity, the necessity claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(virality_necessity_question, conceptual, 'Empirical-contingency of the reading''s core prescriptive claim about viral reciprocity.').

omega_variable(
    contributor_exit_suppression_split,
    'Is the suppressed exit of commons contributors structural (sponsorship leverage, network lock-in of dependent projects, career signaling) or internalized (openness ethos that makes withholding labor feel like betraying the community)?',
    'Post-departure trajectory study: contributors who leave commons work for proprietary employment — does their willingness to condition future contribution on reciprocity recover once the structural levers are removed?',
    'If suppression persists after exit, the effective hold on contributors exceeds the structural measure and the reading''s exploitation diagnosis deepens; if it dissolves, the arrangement''s persistence rests on ordinary incentive alignment rather than capture of persons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_exit_suppression_split, empirical, 'Structural versus internalized component of contributor-side suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(perm_tr_t6, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(perm_tr_t18, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(perm_tr_t36, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 36, 0.31).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(perm_be_t6, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(perm_be_t18, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(perm_be_t36, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 36, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(perm_su_t6, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(perm_su_t18, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(perm_su_t24, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(perm_su_t36, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 36, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'permissive licensing' conflates three structurally distinct claims about the same license texts, so the kernel permissive_license_text decomposes into three linked stories. commons_coordination_reading (upstream, highest empirical confidence in the texts' day-to-day function) authors near-zero extraction; corporate_moat_reading authors high extraction with deliberate-strategy emphasis; this story, copyleft_counterfactual_reading, authors high extraction with a prescriptive counterfactual (viral reciprocity as necessary corrective). Each file carries its own epsilon, beneficiaries, and victims; the upstream coordination reading is typically cited as evidence BY the other two, hence the family edges run from it downward. Changing the observable (whose costs count, which transfers register) changes epsilon — which is the signature that these were never one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
