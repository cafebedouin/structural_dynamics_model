% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow-Linking Reading: Aggregation Doctrine Shielding Unmodified Combination
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   The standing arrangement under contest is the operative legal-industrial
 *   rule that combining an unmodified GPL component with independently
 *   written code by linking constitutes mere aggregation, not the making of a
 *   derivative work, so that only edits to the GPL code itself trigger
 *   license obligations. Under this rule a large proprietary software economy
 *   runs on volunteer-built GPL infrastructure — runtime libraries,
 *   compilers, networking utilities — without returning source, and the
 *   reciprocity promise that motivated the original grants is honored only in
 *   the narrow case of direct modification. The rule does real coordinative
 *   work: it draws a bright, checkable line that lets mixed-license supply
 *   chains interoperate and slashes compliance-analysis costs, and it is not
 *   a fiction — the statutory aggregation category is genuine copyright
 *   doctrine. But the same line functions asymmetrically: the value of the
 *   commons flows inward across it while the source guarantee does not flow
 *   outward, and the arrangement holds in place through an actively
 *   maintained legal posture — retained defense counsel, industry association
 *   positions, and a litigation-cost gradient that chills the counterparties
 *   who would press a stricter trigger. This story instantiates the narrow
 *   reading of the linking kernel as a single epsilon-stable constraint;
 *   epsilon's referent is this narrow-governed arrangement itself, scored as
 *   it actually operates.
 *
 * KEY AGENTS:
 *   - - proprietary_software_vendors: Primary beneficiary (institutional/arbitrage) — collects integration value of GPL components without reciprocal source release; also funds and administers the reading's defense
 *   - - copyright_judicial_establishment: Agenda setter (institutional/constrained) — case-by-case adjudication sets which trigger operates; cannot exit adjudication
 *   - - free_software_contributors: Primary target (moderate/constrained) — grant broad rights expecting reciprocity; past grants irrecoverable
 *   - - fsf_copyleft_stewards: Target and counter-agenda (organized/identity_locked) — propagation program defeated by the operative reading; institutionally fused to the goal
 *   - - copyleft_enforcement_projects: Target (moderate/constrained) — press compliance actions under a persistent resource asymmetry
 *   - - end_users_of_integrated_products: Mixed beneficiary/target (organized/constrained) — gain price and availability, lose inspection and repair rights
 *   - - dual_license_commercial_vendors: Target (organized/mobile) — commercial-license demand depends on the stricter reading staying credible
 *   - - academic_ip_scholars: Analytical observer (moderate/analytical) — maps the aggregation/derivation boundary for every faction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.66).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.62).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow-Linking Reading: Aggregation Doctrine Shielding Unmodified Combination").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'd4161cbf-7a7a-4312-8761-ee80a4ed50bc').
narrative_ontology:cs_kernel_codification('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', fixed_text).
narrative_ontology:cs_authority_grounding('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', distributed).
narrative_ontology:cs_reading_relation('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', foundational, linking_is_mere_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_mere_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', linking_is_mere_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', secondary, obligation_trigger_confined_to_modification).
narrative_ontology:cs_axiom_status(obligation_trigger_confined_to_modification, holdable).
narrative_ontology:cs_axiom_grounding('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', obligation_trigger_confined_to_modification, conventional).
narrative_ontology:cs_reference_frame('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', statutory_aggregation_baseline).
narrative_ontology:cs_drift_state('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', contemporary_litigation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d4161cbf-7a7a-4312-8761-ee80a4ed50bc', '2026-06-15T09:30:00Z').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_integrated_products).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_contributors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_stewards).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_enforcement_projects).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_integrated_products).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, dual_license_commercial_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship commercial products built on GPL-licensed components — runtime libraries, toolchains, system utilities — while releasing none of their own source. Under the operative rule the act of linking imposes no licensing obligations on their code; only edits to the GPL component itself would. They finance the reading's defense through retained counsel, industry-association positions, and litigation posture, and they collect the integration value of decades of volunteer-written infrastructure. Exit is real and exercised: they can purchase commercial licenses, substitute permissively licensed equivalents, or rewrite components, and they shop continuously among these options.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, agenda_setter).

% Adjudicates the comparatively rare infringement claims where a license holder alleges that a linked, unmodified combination infringes. Their rulings — and their reluctance to sweep broadly — set which trigger operates in practice. They are bound to statutory definitions and procedural limits, cannot decline adjudication when litigation arrives, and preside over jurisdictions whose doctrines do not move in lockstep, so no single bench settles the question for the whole ecosystem.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyright_judicial_establishment, agenda_setter,
    institutional, generational, constrained, continental).

% Write and maintain GPL code as volunteers or nonprofit employees, granting broad rights in exchange for the promise that downstream recipients receive source. Under the operative reading their libraries ship inside proprietary products that return nothing; the promised reciprocity arrives only when someone modifies their files. Leaving means stopping contributions or relicensing future versions — rights already granted on past versions cannot be recalled, and community standing attaches to the projects they would have to walk away from.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_contributors, payer,
    moderate, biographical, constrained, global).

% Author and steward the license text, publish interpretations asserting that linking creates derivative works, and spend organizational resources on compliance education and enforcement support. The operative reading defeats their propagation program: code circulates into closed products instead of propagating freedoms. Their institutional self-concept is fused to the propagation mission, which forecloses abandoning the goal even as enforcement repeatedly stalls; they administer the text's asserted meaning even though courts, not they, decide what operates.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_stewards, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_stewards, agenda_setter).

% Pursue compliance actions on behalf of member projects — demanding correspondence, offering cure periods, litigating when refused. Each action costs the enforcing side orders of magnitude more than the defendant's defense; losses and quiet settlements chill subsequent claims. They continue because member projects request it, but the resource gradient caps how often and how aggressively the stricter trigger can be pressed.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_enforcement_projects, payer,
    moderate, biographical, constrained, global).

% Receive polished products that bundle GPL-derived components inside proprietary shells, often at prices made possible by unpaid commons labor. They gain availability and price; they lose inspection, modification, and repair rights for the combined work, and the source guarantee they would hold under a stricter trigger never materializes. Switching products carries real migration and retraining costs, and no channel exists through which they were consulted about the trade.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_integrated_products, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_integrated_products, payer).

% Fund development of popular GPL projects and sell exception certificates and commercial licenses to firms afraid of contamination. When linking alone triggers nothing, the fear driving certificate purchases shrinks; their revenue depends on the stricter reading staying credible. They can and do pivot — support contracts, hosted offerings, open-core splits — so the harm lands on their business model rather than trapping them.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, dual_license_commercial_vendors, payer,
    organized, biographical, mobile, global).

% Map the aggregation/derivation boundary in copyright doctrine, audit statutory text against industrial practice, and publish analyses that serve every faction. They hold no financial stake and no enforcement role; their engagement with the question is entirely elective and costless to exit.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, academic_ip_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a bright, checkable line separating obligation-triggering modification from non-triggering combination: compliance teams can verify adherence by diffing the GPL component rather than proving a derivation relation across a compiled binary, and mixed-license supply chains interoperate without case-by-case legal analysis of every linkage.
% TRANSFER_FUNCTION: Moves functional value produced under a reciprocity promise — volunteer and nonprofit GPL engineering — into proprietary products, while moving back no source and no obligations; incidentally moves enforcement risk and litigation cost onto whichever counterparty presses the stricter trigger.
% ABSENT_VOICES: Hobbyist and small-project contributors who chose the license for its reciprocity guarantee but possess no legal representation; their objection surfaces only vicariously through the stewards and enforcement organizations. Downstream users were never consulted on trading away the source guarantee, and maintainers who lack enforcement capacity have no seat at the adjudications that set the operative rule.
% DISAPPEARANCE_RATIONALE: If the aggregation exemption vanished overnight — if courts uniformly held that linking creates a derivative work — thousands of shipping proprietary products would face immediate compliance crises, triggering a relicensing scramble, a surge in commercial-license and exception purchases, withdrawal or rewriting of products whose economics cannot absorb disclosure, and a redesign of build practices around clean-room boundaries; dual-licensing revenue models would boom while narrow-reading-dependent product lines collapsed. The mixed-licensing software economy is arranged around this rule and would visibly reorganize without it.
% FOUNDING_PROBLEM: When copyleft licensing met commercial software, no one could say with confidence whether combining code across the license boundary created a derivative work under copyright law — vendors feared unbounded liability, stewards feared wholesale appropriation, and mixed ecosystems stalled on unanswerable compliance questions. The arrangement crystallized an answer: treat linking as aggregation, trigger obligations only on modification, and let mixed ecosystems proceed.
% FOUNDING_PROBLEM_CORROBORATION: Court records and published doctrinal analyses from legal academia corroborate that the coexistence question was a genuine, unresolved legal problem rather than a manufactured pretext — judges confronted it in actual infringement disputes, and scholars documented the definitional gap in the statutory derivative-work categories as applied to machine code. Those same sources, sitting outside the beneficiary set, split on whether the current operative resolution answers the founding problem or merely freezes it in favor of appropriators: the enforcement organizations and steward publications attest that the harder half of the problem (protecting reciprocity across combination) remains unsolved, while industry legal literature attests the bright line resolved the tractable half.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66 because the arrangement systematically decouples appropriated value from reciprocity: the brighter and more accepted the aggregation line becomes, the larger the stock of GPL work that ships inside closed products with no source returned, and the measurement series shows that accumulation rising monotonically across the interval as proprietary embedding deepened. Suppression is 0.62 and is authored as a RAW structural property — it is not scaled by power or scope; the engine scales only extractiveness. The suppressive force is structural, not physical: a litigation-cost gradient under which each enforcement attempt costs the enforcing side orders of magnitude more than the defending side, plus cease-and-desist posturing that trades on interpretive ambiguity; the temporal series tracks the maturation of the defense apparatus, which is why suppression_requirement is the enforcement-history metric here. Theater ratio is 0.34: the bright line performs genuine regulative work (compliance teams can actually apply it), but a growing share of 'neutral doctrinal clarification' activity functions as cover for protecting the exemption, which is why theater trends upward without approaching piton levels. Accessibility collapse is low (0.35) because alternatives remain fully live — dual licensing, LGPL-style weakened copyleft, clean-boundary design, permissive-component substitution — and resistance is high (0.72) because steward campaigns, enforcement litigation, and relicensing pressure actively contest the arrangement throughout. All three temporal series run on one shared grid (t=0..24 step 4) so every metric is authored at every examined point; the trajectories are monotone, with no oscillatory cycle, so no intermittent-reinforcement dynamics are claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should classify differently and the engine computes that divergence from the structural data rather than from the authored claim. From the vendor seat the arrangement presents as ordinary legal infrastructure: a bright rule that permits building products, with exit options (commercial licenses, component substitution) that make participation feel chosen. From the contributor seat the same line operates as enclosure: work granted under a reciprocity promise is consumed under terms that void the promise, with exit that forfeits sunk contribution. From the judicial seat neither reading is settled doctrine — the establishment sees an open interpretive question adjudicated case-by-case, which is precisely why the arrangement requires continuous defense. The dual-license vendor seat is the sharpest lateral contrast: nominally inside the open-source economy at similar standing to the beneficiaries, its revenue is destroyed rather than protected by the narrow rule, demonstrating that identical nominal position does not produce identical constraint experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place proprietary_software_vendors at the low-directionality end: they collect the arrangement's gains and hold arbitrage-grade exit (component substitution, negotiated exceptions), sitting nearest the subsidy pole. end_users_of_integrated_products are authored into both arrays deliberately — they receive real availability benefits while carrying the lost source guarantee, landing near the symmetric midpoint with constrained exit pulling them slightly toward the target side. Victim declarations place free_software_contributors near the full-target end: their grants are irreversible, their reciprocity expectation is defeated, and individual exit means abandoning contribution entirely. fsf_copyleft_stewards sit at high directionality with identity_locked exit amplifying effective extraction — the institution cannot rationally abandon the propagation goal its identity is constituted by. copyleft_enforcement_projects carry high directionality with constrained exit (mission-bound but organizationally replaceable). dual_license_commercial_vendors carry moderately high directionality damped by mobile exit: the rule harms their model but they can pivot. The judicial establishment holds no declared beneficiary or victim position; it administers rather than collects, and its seat is computed from its structural role rather than from the arrays.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how may independently developed programs legally combine with GPL-covered works without wholesale relicensing — retains live content: new coupling technologies (link-time optimization, generated code, containerized bundling) continuously reopen the boundary question, so the founding_problem_status is authored as contested rather than dead, and the (contested x world_rearranges) cell raises no zombie flag. The classification discipline matters here in both directions. Calling this a rope would erase the asymmetric transfer the same line performs — coordination and enclosure ride one structure, which is exactly the tangled-rope signature. Calling it a snare would deny the genuine coordination function: the bright line solves a real collective-action problem that every mixed-ecosystem participant, including many contributors, rely on daily. Mandatrophy risk sits elsewhere: if clean-interface architecture and weakened-copyleft variants eventually make the aggregation question moot by construction, the operative reading could persist as pure defensive ritual — the slowly rising theater_ratio series is the early indicator to watch, and the extraction-accumulation trajectory warrants continued temporal monitoring under the abductive accumulation hypothesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_operative_delta,
    'This constraint is the narrow_linking_permissive_reading of kernel gpl_derivative_work_trigger. Which of the three declared readings governs operative obligation-triggering, and what structurally changes under each sibling: under broad_copyleft_reading the proprietary vendors named here become obligation-bearing parties and the source-availability guarantee extends across the link; under interface_boundary_reading a middle regime appears where clean-API combinations stay exempt while tightly coupled ones trigger.',
    'Consolidated appellate precedent on linked-work infringement, or legislative clarification of the derivative-work definition as applied to machine-code combination; until then, track which reading enforcement counterparty behavior actually presupposes.',
    'If the broad reading consolidates, this story''s beneficiary/victim polarity inverts and the arrangement reclassifies toward enforced extraction on vendors; if the interface reading wins, the exemption narrows to provably clean boundaries and the measured extraction redistributes onto tightly coupled stacks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_operative_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would change beneficiary/victim polarity.').

omega_variable(
    linking_derivation_factual_basis,
    'Does machine-code linking ever factually satisfy the statutory adaptation criteria — e.g., link-time optimization, aggressive inlining, header-content absorption, or static embedding producing a genuinely unitary adapted work — or is the narrow reading''s aggregation premise descriptively sound across real build chains?',
    'Forensic audits of shipped build artifacts (symbol tables, merged code sections, derivative debug info) combined with court-commissioned technical expert examination in a contested infringement case.',
    'If modern toolchains routinely blur the modification/linking line, the narrow reading''s factual foundation erodes and its exemption collapses case-by-case toward the broader trigger; if artifacts cleanly separate, the reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_derivation_factual_basis, empirical, 'Whether the aggregation-vs-derivation factual premise holds under contemporary compilation practice.').

omega_variable(
    merit_vs_enforcement_cost_asymmetry,
    'Does the narrow reading prevail because courts judge it doctrinally sound, or because enforcement counterparties lack the resources to test the broader reading at scale?',
    'Outcomes tracking of well-funded enforcement campaigns and appellate treatment of the strongest available cases; convergence of independent courts across jurisdictions on the same verdict would separate merit from attrition.',
    'Sustained wins by better-resourced enforcement would flip the operative reading without any doctrinal argument changing — reclassifying this arrangement''s persistence as coercively maintained rather than consensual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_vs_enforcement_cost_asymmetry, empirical, 'Whether prevalence reflects doctrinal correctness or litigation-cost asymmetry.').

omega_variable(
    end_user_source_value_materiality,
    'How much do end users of integrated proprietary-plus-GPL products actually lose from the evaporated source guarantee — is forgone inspection, repair, and fork capacity materially valuable to them, or largely theoretical?',
    'Revealed-preference studies of user willingness to pay for source-available equivalents, and repair-market evidence where source access enabled third-party maintenance.',
    'High materiality pulls end users from the symmetric middle toward the payer pole, widening the extraction footprint; negligible materiality concentrates the arrangement''s costs on contributors and stewards alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(end_user_source_value_materiality, empirical, 'Materiality of the lost source-availability guarantee to downstream users.').

omega_variable(
    cs_framing_underdetermination,
    'The commitment-system framing chosen here locates adjudication authority in distributed court-plus-practice interpretation of a fixed license text; an alternative framing locates it in the steward''s propagation-intent tradition, under which this reading reads as a defection from the kernel''s declared purpose rather than a competing interpretation of it. Which framing represents the kernel''s actual authority structure?',
    'Signals guiding the current choice: operative adjudication events occur in courts and procurement negotiations, not in steward pronouncements; steward interpretations bind no one outside voluntary adopters. Re-adjudicate if a court ever defers to steward interpretation as controlling.',
    'Under the propagation-intent framing, this reading''s axioms register as overridden-by-the-kernel''s-own-text and the authority structure shifts toward lineage, changing the drift vector and the foreclosure computation against the broad reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two defensible framings of the kernel''s adjudicating authority produce different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_linking_tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gpl_narrow_linking_tr_t4, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(gpl_narrow_linking_tr_t8, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(gpl_narrow_linking_tr_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(gpl_narrow_linking_tr_t16, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(gpl_narrow_linking_tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(gpl_narrow_linking_tr_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 24, 0.34).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_linking_be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(gpl_narrow_linking_be_t4, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(gpl_narrow_linking_be_t8, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(gpl_narrow_linking_be_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(gpl_narrow_linking_be_t16, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(gpl_narrow_linking_be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(gpl_narrow_linking_be_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(gpl_narrow_linking_su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(gpl_narrow_linking_su_t4, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(gpl_narrow_linking_su_t8, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(gpl_narrow_linking_su_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(gpl_narrow_linking_su_t16, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(gpl_narrow_linking_su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(gpl_narrow_linking_su_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial 'GPL linking controversy': the single label conflates three epsilon-distinct claims sharing the kernel gpl_derivative_work_trigger. The broad copyleft reading is upstream (authored intent, highest normative ambition); this narrow permissive reading and the interface boundary reading are downstream operative resolutions whose epsilon values differ sharply — this reading exempts all unmodified linking regardless of coupling tightness (epsilon 0.66, coordination-plus-extraction), while the interface reading exempts only provably clean boundaries (different victim set, different enforcement profile). Each family member links the others via affects_constraints; the upstream broad reading structurally influences both downstream readings by supplying the enforcement threat that gives their exemptions economic value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
