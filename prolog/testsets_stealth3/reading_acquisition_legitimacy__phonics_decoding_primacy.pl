% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics-Decoding Primacy Legitimacy Norm in Beginning-Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Since roughly 2006, a legitimacy norm has consolidated across
 *   English-speaking school systems: reading is decoding, and instruction
 *   that does not teach the alphabetic principle explicitly and
 *   systematically does not count as legitimate beginning-reading
 *   instruction. Codified through statute waves (England's Rose Review and
 *   phonics screening check; U.S. science-of-readising laws beginning with
 *   Mississippi in 2013 and proliferating from 2019), accreditation
 *   standards, curriculum adoption criteria, and mandatory retraining, the
 *   norm governs teachers, preparation programs, publishers, and districts
 *   alike. It displaced a whole-language inheritance and now draws the
 *   professional boundary of respectability in early literacy. The claim and
 *   the metrics are independent authored facts: I claim tangled_rope because
 *   I believe the structure carries BOTH a genuine coordination function (one
 *   definition, one sequence, early flagging) AND asymmetric extraction
 *   through active enforcement; I authored the metric values from what I
 *   believe descriptively true of its operation, without tuning either toward
 *   a predicted engine verdict. KEY AGENTS (by structural relationship): -
 *   struggling_beginning_readers: Intended beneficiaries (powerless/trapped)
 *   — receive the mandated instruction; flagged early when they stall -
 *   parents_of_struggling_readers: Organized beneficiary constituency
 *   (organized/constrained) — drove the statutes; buy private tutoring where
 *   implementation lags - science_of_reading_vendors: Commercial
 *   beneficiaries (organized/arbitrage) — supply mandated training, decodable
 *   texts, and screens; revenue scales with statute count -
 *   balanced_literacy_teacher_educators: Displaced payers
 *   (moderate/identity_locked) — programs reviewed out of approval; careers
 *   fused with the displaced paradigm - classroom_teachers:
 *   Compliance-bearing payers with partial benefit (moderate/constrained) —
 *   retrained, monitored, discretion narrowed -
 *   students_with_comprehension_based_difficulties: Misdirected payers
 *   (powerless/trapped) — screened into code intervention they do not need -
 *   state_literacy_authorities: Agenda setters (institutional/short horizon)
 *   — write and administer the mandates at low personal cost -
 *   reading_science_researchers: Analytical observers with material stakes
 *   (institutional/constrained) — supply the evidence warrant and staff the
 *   panels - early_childhood_developmentalists: Excluded critics
 *   (organized/constrained) — warn on developmental displacement; absent from
 *   hearings and panels
 *
 * KEY AGENTS:
 *   - struggling_beginning_readers: intended beneficiary (powerless/trapped)
 *   - parents_of_struggling_readers: organized beneficiary constituency (organized/constrained)
 *   - science_of_reading_vendors: commercial beneficiary with capture seat (organized/arbitrage)
 *   - balanced_literacy_teacher_educators: displaced payer (moderate/identity_locked)
 *   - classroom_teachers: compliance-bearing payer with partial benefit (moderate/constrained)
 *   - students_with_comprehension_based_difficulties: misdirected payer (powerless/trapped)
 *   - state_literacy_authorities: agenda setter (institutional/immediate)
 *   - reading_science_researchers: observer with material stakes (institutional/constrained)
 *   - early_childhood_developmentalists: excluded critic (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.65).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics-Decoding Primacy Legitimacy Norm in Beginning-Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4').
narrative_ontology:cs_kernel_codification('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', formalized).
narrative_ontology:cs_authority_grounding('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', expertise).
narrative_ontology:cs_interpretation_layer_present('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4').
narrative_ontology:cs_reading_relation('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', foundational, decoding_necessity_gateway).
narrative_ontology:cs_axiom_status(decoding_necessity_gateway, holdable).
narrative_ontology:cs_axiom_grounding('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', decoding_necessity_gateway, empirically_contingent).
narrative_ontology:cs_axiom('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', foundational, explicit_instruction_beats_incidental_acquisition).
narrative_ontology:cs_axiom_status(explicit_instruction_beats_incidental_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', explicit_instruction_beats_incidental_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', explicit_alphabetic_instruction_baseline).
narrative_ontology:cs_drift_state('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', contemporary_science_of_reading_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a8697f9-9a38-4fec-8fed-9d1ea2b4f9d4', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_beginning_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_vendors).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_teacher_educators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_comprehension_based_difficulties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_science_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children in the first years of formal schooling who would otherwise reach upper grades unable to sound out unfamiliar words. Under the current regime they receive daily explicit letter-sound instruction, decodable texts matched to what has been taught, and frequent short screenings. Those who progress gain a durable skill early; those who stall are flagged within months instead of years. They exercise no voice anywhere in the decisions that arrange their instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_beginning_readers, beneficiary,
    powerless, biographical, trapped, global).

% Organized dyslexia and reading-disability advocacy networks that testified for screening laws, litigated for earlier identification, and supplied the political force behind much of the legislation. They regard early code instruction as protection for their children and celebrate early-flagging gains. Where local implementation lags they purchase private tutoring at significant household cost; moving schools or districts is possible but disruptive.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, national).

% Training providers, decodable-book publishers, and screening-tool firms that supply what mandate compliance requires: approved retraining hours, scripted lesson kits, leveled decodable sets, benchmark assessments. Revenue scales directly with the number of statutes passed and curricula adopted. Product lines pivot quickly when standards or approved-provider lists shift; the firms bear little risk from the policies they profit from.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% University faculty whose research programs and preparation courses grew from the whole-language and Clay-Goodman traditions. State review panels now cite cueing instruction and unvetted approaches as disqualifying; preparation programs close, merge, or rename; decades-long research lineages lose publication venues and grant eligibility. Leaving the field would mean abandoning a scholarly identity built over an entire career, so most stay and fight rearguard actions within the institutions that remain.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_teacher_educators, payer,
    moderate, generational, identity_locked, national).

% Practitioners who must complete state-approved retraining hours, deliver sequenced lessons under fidelity walkthroughs, retire materials some had refined for years, and document screening data. Union grievance channels register complaints but collective bargaining rarely reaches curriculum mandates. Many report their decoding instruction genuinely improved; many also report narrowed professional discretion and resentment at being retrained in what they were previously told to unlearn.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, beneficiary).

% Children whose reading difficulty stems from limited vocabulary, thin oral-language background, or comprehension-processing differences rather than letter-sound gaps. Screening regimes route them into tiered phonics intervention targeting skills they largely possess, consuming intervention slots and instructional time while the help they actually need arrives late or not at all. Placement decisions belong entirely to adults; they cannot opt out or redirect their own support.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_comprehension_based_difficulties, payer,
    powerless, biographical, trapped, global).

% Legislatures write the statutes; education agencies maintain approved-provider lists, review preparation programs, certify retraining, and audit district compliance. Political credit accrues immediately from passing reading bills; the fiscal and pedagogical costs of correction fall on successors after office turnover. Personal exposure to failure is minimal because accountability resets with each election cycle.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_literacy_authorities, agenda_setter,
    institutional, immediate, arbitrage, continental).

% Cognitive and education scientists whose syntheses anchor the norm. They testify before panels, sit on technical advisory boards, and lend evidentiary credibility to legislation, gaining citations, advisory seats, and grant lines in return. The boundary between evaluating the regime and staffing it has blurred; career investment now rides on the paradigm remaining ascendant, though several prominent members openly criticize specific mandates and vendor claims.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_science_researchers, observer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_science_researchers, beneficiary).

% Early-childhood specialists who argue that intensive formal drill in kindergarten displaces play, oral-language development, and motivation, particularly for the youngest entrants. They publish cautionary work and speak at their own conferences, but appear rarely in legislative hearings, hold no seat on the technical panels, and carry no leverage over curriculum procurement. Their cautionary case enters the record only when individual legislators happen to solicit it.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_childhood_developmentalists, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_vendors).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real fragmentation problem: before codification, beginning-reading pedagogy varied by district fashion and instructor preference, struggling readers were identified late or not at all, preparation programs trained mutually contradictory methods, and no common operational definition of reading acquisition existed. The norm gives the ecosystem one definition, one build sequence, and a shared early-warning screen.
% TRANSFER_FUNCTION: Moves public money — retraining budgets, curriculum adoptions, screening licenses — from district general funds toward approved training providers and materials publishers; moves instructional authority from classroom professionals toward codified scope-and-sequence documents; moves early-grade instructional time toward explicit code work and away from unstructured engagement with authentic text.
% ABSENT_VOICES: Novice readers themselves are never present in any forum that decides their instruction. Early-childhood developmentalists warning about developmental displacement sit outside the hearings and panels. Veteran whole-language practitioners spoke substantively only before the norm hardened; afterward their participation is confined to defensive testimony. Parents of children harmed by rigid implementations lack an organized channel comparable to the dyslexia advocacy networks.
% DISAPPEARANCE_RATIONALE: If the legitimacy norm and its enforcement vanished overnight, preparation-program approvals would revert, cueing-based and balanced materials would re-enter adoption cycles within a year or two, screening infrastructures would decay toward whatever districts voluntarily maintain, vendor revenue would collapse toward voluntary-purchase levels, and the professional boundary defining respectable reading instruction would dissolve back into jurisdictional and ideological variation — a wholesale reorganization of the early-literacy economy.
% FOUNDING_PROBLEM: Large fractions of children, disproportionately from disadvantaged backgrounds, were finishing elementary school unable to read proficiently; whole-language-era pedagogy left systematic decoding gaps; struggling readers were identified years too late for intervention to be cheap and effective; and instructional method followed ideology rather than evidence, with no accountability for outcomes.
% FOUNDING_PROBLEM_CORROBORATION: NAEP long-term trend data and international assessments attest persistent reading failure independent of any vendor or advocacy interest; the pediatric and neuropsychological literature on late-identified reading disability attests the cost of delayed identification. Nobody credible attests the problem is solved — proficiency rates remain far below stated goals — and the vendors and advocacy groups inside the beneficiary set are precisely the sources whose attestation would not count.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: the regime delivers a real service — early flagging and durable decoding skill for most entrants — while a growing share of mandate spending accrues to approved providers, displaced traditions absorb career losses, and a measurable subset of strugglers receives mismatched intervention. Suppression 0.65, authored RAW and UNSCALED (only extractiveness is scaled by directionality and scope in the engine's arithmetic): several statutes explicitly prohibit cueing strategies, screening regimes and approved-provider lists gate entry, and accreditation reviews disqualify preparation programs — alternatives collapse quickly inside mandate systems (accessibility_collapse 0.60) but persist in private, home, and non-statute jurisdictions. Resistance 0.52: continuing scholarly dissent, union grievance campaigns, occasional parent pushback against rigidity, and litigation over program closures. Theater 0.35: science-of-reading branding, certificate walls, and worksheet relabeling visibly outrun changed classroom practice in many districts. The temporal series run on ONE shared grid (t=0,3,6,9,12,15,18) so every tracked metric is authored at every examined point; all three rise monotonically with no cycle — the dynamic is a legislative ratchet, each statute hardening the last and vendor scaling financing advocacy confidence, so no oscillation mechanism exists to document. Identity-lock dynamics concentrate in the displaced teacher-educator seat: the fusion is professional and ideological (careers and self-concept constituted through the Goodman-Clay lineage), making exit structurally unthinkable even as institutional rewards vanish; if that identity frame broke, the seat's effective pressure would drop sharply and opposition would reorganize rather than dissipate. Suppression mechanism split: predominantly structural (statutes, procurement rules, accreditation — roughly four-fifths of the measured value), with a minority internalized component (practitioners self-censoring formerly routine practices even where unpoliced).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the vendor seat the arrangement is a subsidized market it helped legislate; from the struggling-reader seat it is timely rescue; from the displaced educator seat it is enforced professional obsolescence carried under identity lock; from the legislature seat it is cheap credit-claiming whose correction costs land on successors. Same-nominal-level divergences: teacher educators and classroom teachers both sit at moderate power, but exit differentiates them (identity_locked versus constrained) and review panels name the former while merely auditing the latter; researchers and developmentalists both hold expert standing, but proximity to the evidence warrant — inside the room versus outside it — sets their treatment, not expertise level.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations plus exit options drive the derivation. Vendors sit nearest the full-beneficiary end (arbitrage exit, revenue coupled to statute count). Struggling readers stay low-d despite trapped exit because the beneficiary declaration dominates — the arrangement subsidizes their skill acquisition. Parents sit slightly higher: genuine beneficiaries who additionally absorb tutoring costs where implementation lags. Classroom teachers derive mid-to-high target-side (dual-positioned payer-beneficiary: they bear retraining and discretion losses but gain usable technique). Students with comprehension-based difficulties derive near full-target: trapped, powerless, paying intervention time for services mismatched to their need. Displaced teacher educators derive near full-target with the identity lock amplifying effective pressure. Agenda setters derive low-to-moderate: they spend public funds but collect political returns. Researchers derive mild target-side: attention and independence costs against collected status. Directionality overrides were considered and rejected — the structural declarations already yield the correct ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem (mass reading failure, late identification, ideology-driven method) is verifiably live, corroborated by trend data no beneficiary controls. The tangled_rope classification prevents mislabeling in both directions. Reading the arrangement as pure rope would erase the capture dynamics — the approved-provider economy, the displaced cohorts, the misdirected intervention slots. Reading it as a snare would erase the documented coordination achievement: earlier flagging and measurably improved decoding outcomes in early-adopting jurisdictions. Keeping both faces computable matters because the drift signal lives in the ratio: if vendor-captured margin keeps growing while outcome gains flatten (extractiveness and theater both rising on the shared grid), the structure slides snare-ward and the measurement series will show it; if mandate spending compresses toward genuine service cost, it relaxes rope-ward. Either transition is detectable from the authored data rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation_marker,
    'This constraint instantiates one reading (phonics_decoding_primacy) of the reading_acquisition_legitimacy kernel; which structural elements relocate if a sibling reading were adopted instead?',
    'Compare the sibling stories'' victim sets, coordination claims, and epsilon values; relocation is a structural fact of framing, not a rhetorical preference.',
    'Under whole_language_meaning_primacy the victim set shifts toward novice readers denied meaning-rich text and the coordination claim dissolves; under structured_literacy_remediation the coordination center moves to vulnerable-learners-first diagnostics and the vendor seat shrinks; per-seat classifications recomputed accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_marker, conceptual, 'Committer-frame marker: one reading of a four-reading kernel; sibling readings are separate constraint stories, not parameters of this one.').

omega_variable(
    efficacy_extraction_boundary,
    'How much of the measured cost load is the necessary price of ecosystem-wide coherence, and how much is vendor capture riding on genuinely good science?',
    'Matched-jurisdiction comparisons of outcomes and per-pupil mandate spending between statute states and non-statute states with similar demographics, decomposing cost into coordination component and above-floor margin captured by approved providers.',
    'If most cost sits at or below a reasonable coordination floor, classification trends rope-ward; a wide captured margin confirms the tangled-rope reading and strengthens the capture diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_extraction_boundary, conceptual, 'Whether the extraction component is separable from the coordination component.').

omega_variable(
    non_decoding_struggler_share,
    'What fraction of struggling readers have primary difficulties that are not decoding-based, and therefore absorb misdirected intervention under screening-and-tiering regimes?',
    'Longitudinal screening datasets disaggregating word-recognition profiles from oral-language and comprehension profiles, tracked against intervention assignment.',
    'A small share shrinks the misdirection victim class and softens the asymmetry; a large share deepens the victim side of the ledger and raises effective extraction on the trapped student seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_decoding_struggler_share, empirical, 'Size of the population routed to interventions mismatched to their actual difficulty.').

omega_variable(
    implementation_vs_design_failure,
    'Where mandate-state outcomes disappoint, is the cause the norm''s inherent rigidity (crowding out vocabulary, read-alouds, and motivation) or ordinary implementation failure in under-resourced districts?',
    'Dosage and fidelity studies within statute states correlating implementation quality with outcome variance, controlling for funding and demographic covariates.',
    'High fidelity with flat outcomes implicates the design itself and would justify relaxing enforcement intensity; fidelity-linked variance defends the design and attributes the shortfall to delivery capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_vs_design_failure, empirical, 'Whether observed shortcomings indict the norm or its administration.').

omega_variable(
    naturalized_science_presentation,
    'The norm circulates as settled science under the banner phrase the science of reading; does presenting a policy-coalition product as natural law invite mountain-treatment of a constructed arrangement with identified beneficiaries?',
    'Audit legislative findings clauses against the heterogeneity of the underlying meta-analyses (moderation by learner language background, dosage, age of entry); classify the rhetorical presentation separately from the evidence base.',
    'If the arrangement is ever authored as a mountain declaring beneficiaries, the false-summit signature fires and recomputes toward a hybrid type; keeping the construction explicit in this file preserves the tangled_rope computation and isolates the rhetoric as presentation rather than structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalized_science_presentation, conceptual, 'False-summit risk from naturalizing rhetoric around a constructed policy regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.14).
narrative_ontology:measurement(read_tr_t3, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 3, 0.17).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 6, 0.2).
narrative_ontology:measurement(read_tr_t9, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 9, 0.24).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 12, 0.28).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 15, 0.32).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 18, 0.35).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(read_be_t3, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(read_be_t6, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(read_be_t9, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(read_be_t18, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 18, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(read_su_t3, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 3, 0.44).
narrative_ontology:measurement(read_su_t6, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(read_su_t9, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(read_su_t18, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 18, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The colloquial labels evidence-based reading instruction and the reading wars conflate at least four structurally distinct legitimacy arrangements. Per the epsilon-invariance principle the concept decomposes into a constraint family: one story per reading of the reading_acquisition_legitimacy kernel. This file instantiates the phonics_decoding_primacy reading alone, with its own epsilon authored for the institutionalized phonics-legitimacy regime as this reading assesses it — the referent is never the whole-language arrangement this reading opposed, which belongs to the sibling story. Warrant flow runs upstream from the phonics evidence base into the structured-literacy remediation extension; balanced literacy survives downstream by absorbing phonics components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
