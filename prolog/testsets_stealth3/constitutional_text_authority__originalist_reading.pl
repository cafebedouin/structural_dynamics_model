% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Fixity Constraint on Constitutional Adjudication
 *   domain: constitutional law / legal theory / interpretive jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   constitutional_text_authority: the originalist reading, under which
 *   constitutional meaning is fixed at each provision's ratification and
 *   adjudicative authority derives from the historical public understanding
 *   of that moment. As a governing arrangement, the reading functions as a
 *   standing constraint on adjudication: judges may not resolve
 *   constitutional questions by appeal to contemporary moral judgment or
 *   evolving social attitudes; permissible outcomes are gated by historical
 *   evidence; claims to rights not contemplated in the ratified public
 *   understanding are cognizable only through Article V amendment. The
 *   arrangement solves a genuine coordination problem - it disciplines
 *   judicial discretion against the counter-majoritarian objection and gives
 *   legislatures and citizens a stable expectation of which arguments can
 *   move constitutional outcomes - while running an asymmetric extraction
 *   through the same structure: groups whose claims require reinterpretation
 *   rather than amendment, and groups whose members had no standing in the
 *   ratifying publics whose understanding now binds them, bear costs that
 *   beneficiaries such as legislative majorities and the originalist
 *   professional ecosystem do not. This file decomposes the colloquial label
 *   'how the Constitution gets interpreted' per the epsilon-invariance
 *   principle: the living-constitutionalist and positivist readings are
 *   SEPARATE constraints (separate files) with their own epsilon values,
 *   beneficiary structures, and failure modes, linked through
 *   network.affects_constraints. The epsilon authored here is indexed to THIS
 *   reading's own assessment of the arrangement it instantiates: an honest
 *   originalist concedes the rigidity costs, the foreclosure of unenumerated
 *   claims, and the compounding distance between ratification-era
 *   understandings and present conditions, while weighting them as the
 *   deliberate price of popular sovereignty rather than as rent. The
 *   claim/metric gap is deliberate and independent: claimed_type states the
 *   structure as I believe it to be; the metrics describe the arrangement's
 *   actual operation as descriptively true, and the engine computes per-seat
 *   types from the structural data without reconciling either to the other.
 *
 * KEY AGENTS:
 *   - - originalist_justices: Agenda-setting enforcers (institutional / identity_locked) - apply the fixed-meaning standard, gate outcomes through historical evidence, and police methodological adherence from the bench
 *   - - political_majorities: Primary beneficiary (powerful / constrained) - legislative outputs within the fixed meaning are insulated from substantive judicial revision; they also occasionally pay when courts strike statutes that violate ratified meaning (secondary payer)
 *   - - enumerated_rights_holders: Beneficiary (moderate / constrained) - holders of protections written into the ratified understanding enjoy entrenchment against erosion by later judicial majorities
 *   - - originalist_legal_profession: Beneficiary (organized / identity_locked) - scholars, litigators, and clerkship pipelines whose careers and institutions are constituted by demand for historical-method expertise
 *   - - article_v_adaptive_institutions: Beneficiary (institutional / constrained) - Congress and the state legislatures hold the sole sanctioned channel of constitutional adaptation, concentrating adaptive power in supermajoritarian bodies
 *   - - unenumerated_rights_claimants: Payer (powerless / constrained) - seekers of recognition for rights outside the ratified understanding; partial statutory or state-level substitutes exist, but constitutional recognition requires assembling supermajorities they rarely possess
 *   - - post_ratification_excluded_groups: Payer (powerless / constrained) - groups whose members lacked franchise or civic standing when meanings were fixed and who now live under understandings authored without them
 *   - - living_constitutionalist_jurists: Excluded (institutional / identity_locked) - judges and theorists committed to evolutionary meaning who would object that the method entrenches decisions made without the affected, but who hold no procedural weight inside originalist adjudication
 *   - - academic_constitutional_historians: Observer with secondary beneficiary position (institutional / constrained) - supply the gating evidence and critique lawyer-grade history from within, while enjoying rising demand for their expertise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.46).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.55).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Fixity Constraint on Constitutional Adjudication").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional law / legal theory / interpretive jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'be5e890e-0794-4924-a16d-89478fc3a652').
narrative_ontology:cs_kernel_codification('be5e890e-0794-4924-a16d-89478fc3a652', fixed_text).
narrative_ontology:cs_authority_grounding('be5e890e-0794-4924-a16d-89478fc3a652', lineage).
narrative_ontology:cs_interpretation_layer_present('be5e890e-0794-4924-a16d-89478fc3a652').
narrative_ontology:cs_reading_relation('be5e890e-0794-4924-a16d-89478fc3a652', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('be5e890e-0794-4924-a16d-89478fc3a652', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('be5e890e-0794-4924-a16d-89478fc3a652', foundational, ratified_public_understanding_binding_authority).
narrative_ontology:cs_axiom_status(ratified_public_understanding_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('be5e890e-0794-4924-a16d-89478fc3a652', ratified_public_understanding_binding_authority, deontological).
narrative_ontology:cs_axiom('be5e890e-0794-4924-a16d-89478fc3a652', secondary, constitutional_adaptation_requires_article_v_amendment).
narrative_ontology:cs_axiom_status(constitutional_adaptation_requires_article_v_amendment, holdable).
narrative_ontology:cs_axiom_grounding('be5e890e-0794-4924-a16d-89478fc3a652', constitutional_adaptation_requires_article_v_amendment, conventional).
narrative_ontology:cs_reference_frame('be5e890e-0794-4924-a16d-89478fc3a652', ratification_public_understanding_baseline).
narrative_ontology:cs_drift_state('be5e890e-0794-4924-a16d-89478fc3a652', contemporary_methodological_critique_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be5e890e-0794-4924-a16d-89478fc3a652', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, political_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, enumerated_rights_holders).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_profession).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, article_v_adaptive_institutions).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, post_ratification_excluded_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, academic_constitutional_historians).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, political_majorities).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, popular_sovereignty_consent_theory).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, counter_majoritarian_restraint_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on the courts that apply the fixed-meaning standard. Each decides which historical sources count, how much determinacy the record must show before a question is foreclosed, and when a claim fails for lack of ratification-era warrant. Their published reasoning and confirmation records are built on methodological allegiance; reversing course mid-career would discredit their own prior opinions and votes, so the method is not something they can step outside at will. They gain agenda control and doctrinal legacy from administering the gate; they do not collect money from it.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_justices, agenda_setter,
    institutional, biographical, identity_locked, national).

% Elect legislatures whose enactments are shielded from substantive judicial revision so long as they stay within the ratified meaning. The insulation is worth a great deal in ordinary lawmaking: programs and regulations that a morally adventurous court might strike survive. The same majorities occasionally pay when courts enforce the ratified meaning against their statutes - an enactment that offends the historical understanding falls even if a contemporary consensus supports it - and their only lawful response is to win elections repeatedly or to amend.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, political_majorities, beneficiary,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, political_majorities, payer).

% Hold protections written into the ratified public understanding - speech, religion, arms, criminal-process guarantees, property. For them the fixed-meaning standard is entrenchment: what was won at ratification cannot be eroded by a later judicial majority that finds the protection outdated. Their protection is only as strong as the historical record behind each provision, which makes them invested consumers of historical argument rather than passive recipients.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, enumerated_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% Scholars, specialized litigators, think-tank networks, and clerkship pipelines whose careers, journals, conferences, and placement networks exist because adjudication demands ratification-era expertise. Demand for their output rises with the method's dominance. Their professional identity is constituted by commitment to the method; abandoning it would forfeit their accumulated standing, so they defend it as scholars defend any paradigm.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_profession, beneficiary,
    organized, biographical, identity_locked, national).

% Congress and the state legislatures together hold the sole sanctioned channel through which constitutional meaning can change under this arrangement. Every adaptation that cannot be accomplished by historical argument must pass through them at supermajority thresholds. This concentrates adaptive power in bodies that already hold legislative power, and gives them a structural veto over all judicially-unrecognizable rights claims.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, article_v_adaptive_institutions, beneficiary,
    institutional, generational, constrained, national).

% Seek constitutional recognition for claims the ratifying publics did not contemplate - digital-age informational rights, novel equality claims, emerging dignitary interests. Under the fixed-meaning standard their claims fail in court unless the historical record happens to support them. Partial substitutes exist: statute, state constitutions, ordinary legislation - but none delivers constitutional entrenchment, and the amendment route requires assembling supermajorities they almost never possess. Their realistic option set is lobbying for statutory stopgaps that a later legislature can repeal.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, constrained, national).

% Groups whose members lacked franchise or civic standing when the relevant meanings were fixed - and whose descendants now live under understandings authored in their absence. Even where later amendments rewrote the text, the method's reliance on ratification-era public understanding means the interpretive baseline was set by publics that did not include them. Corrective change is available only through the same supermajoritarian amendment channel, across generations, at costs their coalition rarely sustains.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, post_ratification_excluded_groups, payer,
    powerless, generational, constrained, national).

% Judges and theorists committed to evolutionary meaning. Inside adjudication governed by the fixed-meaning standard, their mode of argument carries no procedural weight except as a foil: they may dissent, publish, and teach, but they cannot move outcomes. Their objection - that the method entrenches decisions made without the affected and freezes moral learning out of fundamental law - is heard only as a position to be defeated, not as a vote. Their professional identities are as fused to their method as their opponents' are to theirs.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_jurists, excluded,
    institutional, generational, identity_locked, national).

% Supply the archival evidence on which the gate turns, and increasingly contest lawyer-grade history in print - documenting selective citation, anachronistic sourcing, and outcome-driven reconstruction in judicial opinions. Their market position improves with the method's dominance even as their findings embarrass it, leaving them in a double bind: professional gain tied to the arrangement they document as degraded.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, academic_constitutional_historians, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, academic_constitutional_historians, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Disciplines constitutional adjudication to a shared, publicly inspectable evidentiary standard: judges, legislatures, and citizens coordinate on which arguments can legitimately move constitutional outcomes, replacing each judge's private moral judgment with a common historical referent and stabilizing expectations about the durability of both statutes and rights.
% TRANSFER_FUNCTION: Moves adjudicative authority from sitting judges' contemporaneous moral judgment to the historical public understanding of ratifying generations; moves the cost of constitutional adaptation onto groups needing unenumerated or corrective recognition, who must assemble Article V supermajorities; and transfers insulation value to legislative outputs that stay within the fixed meaning.
% ABSENT_VOICES: Unenumerated-rights claimants and post-ratification excluded groups would object that the binding understandings were authored without them and that the method prices their recognition out of reach; living-constitutionalist jurists would object that the arrangement freezes moral learning out of fundamental law. Where are they? The claimant groups are present only as litigants whose claims arrive pre-filtered by the historical gate; the excluded jurists are present only as dissents and dissenting scholarship with no procedural vote. Both voices are structurally inside the conversation as objects and outside it as authors.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning standard vanished overnight, judicial discretion would re-expand immediately, pending claims currently foreclosed for lack of ratification-era warrant would become cognizable, and appointment politics would lose its central axis - a genuine rearrangement of the adjudicative world. Defenders answer that nothing catastrophic follows: courts exercised common-law judgment for two centuries before the method consolidated, and the republic survived. The parties genuinely dispute which prediction is right, so the verdict is contested rather than asserted.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: unelected judges with life tenure nullifying the enactments of elected majorities on the basis of contested personal moral premises, eroding the legitimacy of judicial review and destabilizing the settlement that lets a written constitution and democratic self-government coexist.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the legitimacy-of-judicial-review problem is attested across the interpretive spectrum, including by living-constitutionalist theorists who reject this reading's remedy while conceding the difficulty it answers, by political scientists studying court-legitimacy surveys, and by recurring cross-branch conflict over judicial power. Note the corroboration is for the PROBLEM's liveness, not for this arrangement as its solution - critics attest the problem and dispute the cure, which is exactly what distinguishes corroboration from endorsement.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, contested).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).
:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.46: moderate but real. The reading's own lights acknowledge three cost streams - foreclosure of unenumerated recognition, entrenchment of meanings authored under exclusive franchises, and the widening gap between eighteenth/nineteenth-century semantic worlds and present conditions - while treating them as priced features, not defects; hence epsilon sits well below a hostile reading's estimate of the same arrangement but clearly above zero. Suppression is 0.55 and is a raw structural property, deliberately unscaled by power or scope in authorship: the constraint maintains itself through appointment incentives, confirmation-record exposure, journal and clerkship gatekeeping, and bench-level method policing, with a further internalized component carried by identity-fused jurists. Theater_ratio is 0.38: the historical gate performs real verification work in a majority of applications, but a substantial minority of invocations show the signature of motivated reconstruction - selective sourcing, outcome-first reasoning dressed as archival discovery ('law office history') - and that share grows as the stakes of constitutional questions rise. Accessibility_collapse is 0.58: inside adjudication where the reading governs, alternative methodologies have largely collapsed as admissible argument, yet the evolutionary alternative persists institutionally in dissenting opinions, academy, and appointment politics, so alternatives are suppressed but not eliminated. Resistance is 0.62: the constraint meets continuous, organized contestation from living-constitutionalist jurists, critical scholars, and the claimant groups it forecloses, and every judicial appointment cycle is fought substantially over this constraint. The temporal series run on one shared six-point grid; all three tracked metrics rise monotonically across the interval, modeling enforcement-infrastructure maturation (suppression_requirement), compounding exclusionary cost as society recedes from ratified understandings (base_extractiveness), and growth of motivated-history share as stakes increase (theater_ratio). The series is deliberately not cyclical: unlike crisis-driven interpersonal arrangements, this constraint ratchets rather than oscillates, with appointment cycles modulating intensity around a rising trend rather than producing full relaxation phases.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute sharply different types from identical structural data. From the unenumerated_rights_claimant and post_ratification_excluded_group seats, the arrangement operates as enforced foreclosure: a gate they cannot pass, administered by actors who did not ask them, with exit routed through a supermajoritarian channel they cannot assemble. From the political_majorities and enumerated_rights_holders seats, the same structure is protective coordination: insulation of democratic outputs and entrenchment of won protections. From the originalist_justices seat, it is fidelity: the discipline that makes judicial power tolerable. The excluded living_constitutionalist_jurists seat experiences the constraint primarily through its suppression machinery - delegitimation of their methodology - rather than through direct extraction, which is why their position registers through the resistance and suppression metrics rather than through a directionality assignment. The engine computes this divergence per seat; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: political_majorities (damped further toward subsidy by their concentrated, immediate insulation gain, though their secondary payer position pulls them back from the full-beneficiary pole - they occasionally fund the constraint's operation by losing statutes that violate ratified meaning); enumerated_rights_holders (entrenchment beneficiaries with modest power); originalist_legal_profession (career rents from method demand); article_v_adaptive_institutions (concentration of the sole adaptation channel). Victim declarations drive the high-d seats: unenumerated_rights_claimants and post_ratification_excluded_groups, both powerless with constrained exits - the derivation places them near the full-target pole, and the engine scales their effective extraction upward accordingly. The originalist_justices derive near the beneficiary end: they administer the gate and their authority depends on it, though they collect prestige and agenda control rather than material rent. Excluded jurists are deliberately NOT placed in the beneficiary or victim arrays: their structural position is absence from the conversation, and their experience is captured through the stakeholder layer and the suppression/resistance metrics instead. No directionality overrides are used: the override mechanism is keyed to power atoms, and this story contains multiple distinct institutional-power actors (justices, excluded jurists, amendment-channel holders) whose differentiation comes from declared roles and relationships, not from power level - an atom-level override would flatten precisely the distinctions the story exists to measure.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification as tangled_rope is what prevents mislabeling in both directions. Read without the category apparatus, the arrangement is typically described as either pure virtue ('judicial restraint and popular sovereignty') - which would license treating it as costless coordination and ignoring the compounding foreclosure costs borne by identifiable groups - or pure vice ('rights lockout by dead hands') - which would erase the genuine coordination function that disciplined judicial discretion and stabilized interbranch expectations, a function that existed before the extraction accumulated. Tangled_rope keeps both faces structurally visible: the coordination function is real (the counter-majoritarian problem the arrangement addresses has not been solved by any competitor), and the extraction is real (it runs through the same fixed-meaning gate that performs the coordination, which is exactly the tangled-rope signature). On mandatrophy: the founding problem - the counter-majoritarian difficulty of unelected judges nullifying majoritarian enactments on contested moral premises - remains LIVE; the arrangement has therefore not outlived its mandate, and no mandatrophy resolution is declared. The R5 interview records founding_problem_status as live with cross-spectrum corroboration, and the disappearance verdict as contested, so the status-times-verdict mismatch consumer finds no dead-mandate-plus-dependence flag to fire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (originalist_reading) of the kernel constitutional_text_authority; what would the sibling readings (living_constitutionalist_reading, positivist_reading) change structurally if instantiated instead?',
    'Compare the sibling constraint stories directly: the living-constitutionalist reading relocates extraction toward rigidity costs borne by contemporary majorities and equality claimants, with a beneficiary set weighted toward courts as adaptive interpreters; the positivist reading removes the historical-evidence gate entirely, dissolving the theater question and re-keying legitimacy to enactment procedure.',
    'Sibling instantiation would change the victim sets, the directionality profile of the judicial seat, and whether the historical-evidence gate counts as functional verification or performative maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame omega: which reading of the text-authority kernel is instantiated determines the constraint''s entire structural profile.').

omega_variable(
    historical_record_determinacy,
    'Is the ratification-era public understanding sufficiently determinate to gate permissible outcomes, or is it indeterminate enough that interpreters select among plausible historical accounts to reach preferred results?',
    'Paired-outcome studies: identify disputed questions where competent historians reach divergent reconstructions of the relevant public understanding, then measure whether judicial outcomes track the selection among accounts rather than the evidence itself.',
    'If the gate is systematically indeterminate, the measured theater_ratio understates performativity and the constraint operates far closer to theatrical maintenance than its authored 0.38 indicates; if determinate, the theater ratio is honest and the gate is doing real verification work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_determinacy, empirical, 'Whether the historical-evidence gate discriminates or merely decorates outcomes.').

omega_variable(
    entrenchment_of_past_exclusion,
    'Does fixing meaning at moments when the ratifying publics excluded whole classes from franchise constitute extraction from those classes'' descendants, or is Article V supermajoritarian amendment a legitimate safeguard that prices constitutional change correctly?',
    'Normative-political analysis combined with comparative data: examine whether excluded-class claims that eventually succeeded did so through amendment channels at feasible rates, and whether the parties to the dispute converge or remain split on the fairness of the pricing.',
    'Resolution toward ''extraction'' pushes the constraint snare-ward (coordination story as cover for entrenching past power); resolution toward ''safeguard'' supports the rope-side reading in which the extraction is the deliberate price of popular sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_of_past_exclusion, preference, 'Whether the rights-lockout cost is extraction or the intended price of the arrangement.').

omega_variable(
    method_enforcement_internalization,
    'How much of the constraint''s suppressive force is structural (appointment incentives, journal and clerkship gatekeeping, confirmation-record exposure) versus internalized (judicial and scholarly identity fused with methodological allegiance)?',
    'Career-trajectory analysis of jurists and academics who changed institutional positions or lost enforcement exposure: if methodological allegiance persists after structural pressure is removed, a large internalized component exists; if allegiance tracks position incentives, the suppression is mostly structural.',
    'If largely internalized, effective suppression exceeds the structural measure and persists even if appointment incentives reverse; if largely structural, reform of appointment and gatekeeping channels would rapidly lower the constraint''s coercive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(method_enforcement_internalization, empirical, 'Structural versus internalized split of the constraint''s enforcement pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__originalist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__originalist_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__originalist_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__originalist_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__originalist_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__originalist_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__originalist_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__originalist_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__originalist_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__originalist_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__originalist_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__originalist_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional interpretation' decomposes per the epsilon-invariance principle into three structurally distinct constraints instantiating one kernel (constitutional_text_authority). This file is the originalist_reading member: meaning fixed at ratification, historical public understanding as authority. The living_constitutionalist_reading member carries a different epsilon (its own lights weight rigidity costs and majoritarian subordination differently), a different victim set (contemporary majorities and equality claimants rather than unenumerated claimants), and a different enforcement profile. The positivist_reading member dissolves the historical-evidence gate entirely and keys validity to enactment procedure. The upstream/downstream structure runs through shared text and shared appointment politics: whichever reading holds the bench reshapes the operating environment of the others, which is why all three files link mutually through network.affects_constraints rather than standing alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
