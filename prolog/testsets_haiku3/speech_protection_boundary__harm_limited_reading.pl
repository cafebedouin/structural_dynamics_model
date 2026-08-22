% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Conditional on Harm Limitation
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The harm-limited reading of the speech protection boundary holds that
 *   First Amendment protection is conditional on absence of significant harm
 *   to dignity, equality, and freedom from harassment. Under this reading,
 *   the state may categorically exclude from protection speech that
 *   subordinates marginalized groups, organizes coordinated harassment, or
 *   deploys coded dehumanization. The reading reconceptualizes the
 *   relationship between equality and free speech: rather than treating them
 *   as tensions to be balanced, it treats dignity and equal standing as
 *   substantive conditions on the scope of speech protection. This
 *   instantiation of the contested kernel assumes the state acts in good
 *   faith; it delegates enormous interpretive authority to courts and creates
 *   a permanent boundary-drawing practice where each new form of harm
 *   (algorithmic amplification of hate speech, coordinated harassment via
 *   digital networks, AI-generated deepfakes of subordinated groups) will
 *   need to be adjudicated as 'significant' or not. The measurement series
 *   tracks the constraint's evolution from an emerging doctrinal position
 *   (low extractiveness at t=0, high resistance from absolutist tradition) to
 *   a stabilized legal regime (higher extractiveness at t=40, suppression
 *   requirement plateaus, theater ratio rises as boundary-drawing becomes
 *   routine institutional practice rather than urgent moral correction).
 *
 * KEY AGENTS:
 *   - historically_marginalized_groups: powerless, trapped; gain legal standing and protection against coordinated hate and harassment
 *   - state_authority_structure: institutional, arbitrage; gains gatekeeping authority and enforcement discretion over the speech/harm boundary
 *   - speech_absolutists: moderate, constrained; lose categorical floor; their advocacy becomes conditional rather than governing
 *   - government_critics: moderate, constrained; face risk of overly broad harm definitions targeting dissent and opposition
 *   - judicial_gatekeepers: institutional, arbitrage; carry the burden of adjudicating specific cases and managing the perpetual boundary
 *   - civil_liberties_organizations: organized, mobile; pay through litigation costs and chilling-effect risks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.71).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Conditional on Harm Limitation").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '40d4fe0f-86d0-4f57-bec0-191b0f8d73e3').
narrative_ontology:cs_kernel_codification('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', fixed_text).
narrative_ontology:cs_authority_grounding('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', lineage).
narrative_ontology:cs_interpretation_layer_present('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3').
narrative_ontology:cs_reading_relation('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', foundational, equal_dignity_as_structural_precondition).
narrative_ontology:cs_axiom_status(equal_dignity_as_structural_precondition, holdable).
narrative_ontology:cs_axiom_grounding('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', equal_dignity_as_structural_precondition, deontological).
narrative_ontology:cs_axiom('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', foundational, speech_subordination_as_distinct_harm).
narrative_ontology:cs_axiom_status(speech_subordination_as_distinct_harm, holdable).
narrative_ontology:cs_axiom_grounding('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', speech_subordination_as_distinct_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', equal_dignity_precondition_to_speech).
narrative_ontology:cs_drift_state('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', contemporary_post_2015_digital_hate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('40d4fe0f-86d0-4f57-bec0-191b0f8d73e3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, historically_marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_authority_structure).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speech_absolutists).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, government_critics_under_broad_harm_definitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_group_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, civil_liberties_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of historically marginalized communities gain legal recourse and social enforcement against coordinated speech campaigns designed to dehumanize, harass, or organize violence against their group. Hate speech doctrines recognize the speech-as-subordination mechanism: speech that forecloses exit options and safety. They benefit from a reading that recognizes dignity and equality as coordinate constitutional values, not subordinate to speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, historically_marginalized_groups, beneficiary,
    powerless, generational, trapped, national).

% Acquires legitimate gatekeeping authority over the speech/harm boundary: courts and legislators can now prohibit categories of speech on the basis of harm-to-dignity and equality grounds. The authority structure expands at the boundary where absolutist frameworks would see none. This authority is also the enforcement mechanism—the state must define what counts as 'significant harm,' which creates discretion, capture risk, and asymmetric enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, state_authority_structure, beneficiary).

% Lose the principled legal floor they previously held: near-absolute protection except for imminent lawless action. Under this reading, entire categories of speech (hate speech, coordinated harassment, certain coded dog whistles) are now subject to prohibition on dignity/equality grounds. Their advocacy for categorical speech protection becomes a contested, conditional position rather than the default legal rule.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speech_absolutists, payer,
    moderate, biographical, constrained, national).

% Face the practical risk that 'significant harm to dignity, equality, and freedom from harassment' can be applied expansively to government critics, dissidents, and opposition groups. Where the state holds enforcement authority and incentives are to suppress opposition, the boundary between legitimate criticism and prohibited harm-speech becomes a site of abuse. Their speech remains formally protected only so long as the state authority defines their category as 'not significantly harmful'—a contingent protection.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, government_critics_under_broad_harm_definitions, payer,
    moderate, biographical, constrained, national).

% Bear the burden of adjudicating harm claims in specific cases: must determine whether particular speech constitutes 'significant' harm to dignity or equality, whether harassment is present, whether a dog whistle is 'coded' enough to count. The reading delegates enormous interpretive discretion to courts and creates a perpetual boundary-drawing practice where each case either clarifies or further muddies the standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, judicial_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% The doctrinal tradition centered on Brandenburg-standard near-absolute protection is still present and litigated but loses the presumptive legitimacy this reading grants to harm-based exceptions. The tradition is not foreclosed—courts have not resolved the kernel definitively—but its standing as the governing principle is challenged by the harm-limited reading's explicit alternative.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_legal_tradition, excluded,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(speech_protection_boundary__harm_limited_reading, absolutist_legal_tradition).

% The doctrinal tradition that weighs speech interests against other constitutional values case-by-case provides a methodological cousin to this reading but differs in gatekeeping structure: balancing reserves flexibility to weight in every case, while harm-limitation tries to establish categorical rules. The two traditions compete for methodological primacy in how courts adjudicate speech claims.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, balancing_legal_tradition, observer,
    powerful, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_boundary__harm_limited_reading, balancing_legal_tradition).

% Organizations and advocates for historically marginalized communities champion this reading because it names speech-as-subordination as a legitimate constitutional concern and gives legal teeth to exclusion from harassment. They argue this reading makes dignity and equality substantive, not merely formal, constitutional protections.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_group_advocates, beneficiary,
    organized, generational, mobile, national).

% Typically oppose broad harm-based exceptions because they worry about state abuse and the chilling effect on legitimate speech. They bear the burden of defending speech they consider harmful or offensive because the principled distinction—preventing imminent lawless action—would collapse under a broader harm standard. Their litigation strategy costs institutional resources.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_organizations, payer,
    organized, generational, mobile, national).

% The ICCPR and international human rights bodies recognize harm to dignity and equality as legitimate limits on speech protection (Article 20). This reading aligns the U.S. constitutionalism with international norms; from this observational seat the harm-limited reading appears as a convergence with global practice rather than a narrowing of rights.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, international_human_rights_regime, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, state_authority_structure).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates protection for historically marginalized groups against speech-as-subordination (harassment, hate campaigns, coded dehumanization) while maintaining legitimate state authority to enforce equality and dignity protections. Solves a collective action problem: individual dignity and equal standing cannot be unilaterally self-defended against coordinated speech campaigns designed to foreclose exit and normalize subordination.
% TRANSFER_FUNCTION: Transfers legal authority from speakers (who held near-absolute protection under Brandenburg) to state gatekeepers (who now adjudicate harm-to-dignity and equality grounds). The state gains gatekeeping power; speech absolutists lose the categorical floor; marginalized groups gain legal standing to challenge coordinated harassment. Dignity and equality resources flow toward historically excluded groups via legal protection and social enforcement of dignity norms.
% ABSENT_VOICES: Authoritarian regimes that weaponize harm-based exceptions to suppress opposition are structurally outside the conversation. So are speakers in vulnerable positions whose speech could be miscategorized as harmful by a captured state apparatus. The harm-limited reading's legitimacy in a democratic context assumes good-faith gatekeeping; the absence of bad-faith speakers from the design rationale is itself signal that the constraint's vulnerability lies in implementation, not principle.
% DISAPPEARANCE_RATIONALE: If this reading and its legal instantiation vanished, marginalized communities would lose legal tools to address coordinated hate campaigns, courts would revert to near-absolute Brandenburg protection, and the coordination problem of speech-as-subordination would default back to civil society and market solutions—which historically have failed to protect groups without power. The constitutional arrangement of equality and dignity would rearrange dramatically.
% FOUNDING_PROBLEM: The founding problem is twofold: (1) empirically, coordinated speech campaigns designed to dehumanize and subordinate marginalized groups function as a speech mechanism that forecloses their exit options, equal standing, and safe participation in public discourse—speech-as-subordination is a structural phenomenon, not merely offensive speech; (2) doctrinal, the Brandenburg standard created a gap where equality and dignity harms lacked constitutional remedy despite being coordinate constitutional values. The harm-limited reading names this gap and proposes the remedy.
% FOUNDING_PROBLEM_CORROBORATION: Scholarly consensus from critical race theory, feminist jurisprudence, and international human rights experts outside the U.S. absolutist tradition corroborates the empirical component: coordinated hate speech does subordinate and foreclose equal participation. The doctrinal component is contested: absolutist scholars deny the gap exists, arguing Brandenburg already covers the relevant harms. Testimony from historians of speech doctrine and comparative constitutional scholars documents the gap's presence in other democracies. Civil society data on harassment campaigns and coordinated hate speech show real-world patterns the Brandenburg standard does not reach.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.45→0.68) models the reading's institutionalization: initially, it emerges as a doctrinal alternative competing against a well-established absolute standard (low extractiveness, high resistance). As courts adopt it and boundary-drawing becomes routine practice, the constraint's extractive character becomes clearer—the state's gatekeeping authority expands, critics face real suppression risk, and absolutists permanently lose the categorical floor. Extractiveness plateaus at t=25 (0.68) because the constraint stabilizes: the categories of unprotected speech are relatively clear by then (hate speech, coordinated harassment, certain dog whistles), and further expansion meets organized resistance. Suppression requirement (0.48→0.71) tracks the state's enforcement capacity over the same interval: as the reading hardens into legal doctrine, the state must build administrative capacity to adjudicate harm claims, which requires institutional growth and enforcement discretion. The plateau reflects a stable regime where suppression is high but not intensifying (no further institutional buildout). Theater ratio (0.22→0.42) indicates the rising performative component: as boundary disputes multiply and each new form of speech must be litigated as harm-or-not-harm, courts develop routine language about dignity and equality. Some of this language is genuine constraint-justification; some is rhetoric maintaining institutional legitimacy for gatekeeping. The plateau suggests theater stabilizes around 42%: genuine justice work and performative maintenance coexist in stable proportion. Accessibility collapse is elevated at the structural level (0.58) and rises with class-level dynamics: once the reading is established, individuals and dissident groups see their exit options constrained (cannot simply advocate absolutism without legal contradiction), organizations see alternatives narrowed (must navigate harm doctrine), and classes aligned with marginalized groups see opportunities opened (their dignity gains legal recognition). On the coercion grid: individual-level accessibility is lowest initially (0.42, individuals can still argue absolutism) and rises only slowly (0.52 at t=40) because the reading operates at doctrinal and state levels, not immediately at personal scale. Class-level accessibility is highest (0.68→0.72) because the constraint explicitly targets class-level subordination through hate speech and coordinated harassment—the classes of marginalized groups who are the constraint's beneficiaries see their field of action constrained by the very definition of significant harm. Resistance is strongest at the organizational level (civil liberties groups, absolutist legal organizations) and remains steady (0.75→0.72), indicating persistent organized opposition despite the constraint's institutionalization. Structural resistance (0.73 throughout) reflects the kernel's fundamental contestedness: the question of whether equality and dignity are preconditions or balancing values remains genuinely open.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute dramatically different types from the same structural data. From the marginalized-group seat (beneficiary), this reading appears as a genuine coordination solution: state recognition of dignity as a precondition on speech solves a collective action problem where individuals cannot self-defend against coordinated subordination. From the absolutist seat (payer), the same rule appears as a constraint with high effective extraction: loss of categorical protection, subordination of speech interests to administrative gatekeeping, chilling effect on legitimate speech. From the state authority seat (agenda-setter), the constraint is a natural exercise of legitimate power: enforcing dignity and equality. From the government-critic seat (also payer but different power and exit profile than absolutists), it appears as a Trojan horse: formal dignity protection that creates cover for suppression of dissent. The engine computes these divergences from the structural inputs—beneficiary/victim declarations and power/exit atoms—and produces per-seat classifications. The claim and the metrics are intentionally independent: the claim is that this reading is a genuine coordination mechanism (tangled rope framing from advocates' perspective) with real coordination function (protecting marginalized groups' equal standing) AND asymmetric extraction (gatekeeping authority for the state, loss of protection for absolutists and critics). The metrics reflect the structural reality of high extractiveness and suppression—this is not a claim-metric disagreement but a claim-metric coherence revealing the tension internal to the reading itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: marginalized_groups (d ≈ 0.1–0.2, powerless, trapped exit) are full beneficiaries—the reading explicitly protects their dignity and equal standing, they have no exit option from the harm (trapped by membership in the targeted group), and the constraint's whole function is to benefit them. State authority (d ≈ 0.3–0.4, institutional, arbitrage) is a secondary beneficiary—it gains gatekeeping authority and institutional power, but it also bears the cost of adjudicating disputes and faces political pressure from multiple sides. Payer directionality: speech_absolutists (d ≈ 0.75–0.85, moderate, constrained) are near-complete targets—they lose the categorical floor, their legal strategy is defeated, they must litigate every instance under the new harm doctrine. government_critics (d ≈ 0.70–0.80, moderate, constrained) are targets but with an important caveat: they formally retain protection if their speech does not meet the harm threshold, but the threshold uncertainty creates a suppression effect (perceived risk is higher than formal risk). civil_liberties organizations (d ≈ 0.65–0.75, organized, mobile) are payers—they bear litigation costs and oppose the regime on principle—but they retain mobility and options in a way trapped individuals do not. The directionality chain is: beneficiary/victim declarations (marginalized groups gain standing, critics and absolutists lose categorical protection) + power atoms (institutional state power dominates) + exit options (powerless marginalized groups are trapped, absolutists and critics are constrained, organizations retain some mobility) → the engine derives d values from these inputs. No overrides are needed; the derivation produces the right directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (speech-as-subordination forecloses equal participation for marginalized groups, and Brandenburg leaves this gap unaddressed) remains live and was the original justification for the harm-limited reading. However, as the reading institutionalizes, a secondary mandatrophy risk emerges: the boundary-drawing practice develops its own inertia and institutional constituency (courts, administrative agencies, legal scholars invested in harm doctrine). If the original problem were substantially solved (coordinated hate speech on digital networks were effectively suppressed, marginalized groups' equal standing were secured), the constraint might persist as an administrative habit rather than a live solution. The measurement data show no sign of this yet—suppression and theater ratio plateau rather than declining, suggesting the constraint still serves a real function. But the rising theater ratio (22%→42%) indicates a growing performative component: as the constraint matures, courts increasingly issue boundary-clarifying rulings that seem to reaffirm commitment to dignity protection while actually managing political controversy. The risk that distinguishes mandatrophy from genuine ongoing coordination is whether the state's gatekeeping authority becomes captured—whether harm doctrine drifts from protecting marginalized groups to suppressing government critics under the cover of 'significant harm to social cohesion' or 'harassment of public figures.' The constraint avoids mandatrophy so far because the original problem remains live (marginalized communities still face coordinated subordination) and the beneficiary group retains organized representation (civil rights organizations, marginalized communities themselves). If either condition changed—if hate speech were entirely suppressed, or if marginalized communities became politically dominant and no longer needed the protection—the constraint would be a candidate for mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_contestation,
    'What counts as ''significant harm'' to dignity and equality? How much harm is significant? Who decides, and on what grounds?',
    'The constraint''s jurisprudence would need to produce a stable body of doctrine defining harm categories and thresholds. International comparison to E.U. hate speech regimes and Canadian doctrine would provide natural experiments in where the boundary settles.',
    'If the harm threshold narrows (only explicit genocide incitement, graphic violence threats), the reading converges toward balancing doctrine and the extracted authority from absolutists is returned—classification would shift tangled_rope→rope. If the threshold expands (offense, microaggressions, class-based criticism), the extractiveness rises sharply and the reading risks capture by majoritarian or authoritarian impulses—classification could drift tangled_rope→snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_threshold_contestation, empirical, 'The definitional stability and drift of ''significant harm'' determines whether the constraint''s gatekeeping remains legitimate or becomes capture-prone.').

omega_variable(
    state_capture_risk,
    'Will the state authority use harm doctrine to suppress legitimate government criticism, dissent, and opposition speech?',
    'Longitudinal analysis of harm prosecutions over 10+ years: are they concentrated on marginalized-group protection cases, or do they drift toward suppressing dissidents and critics? International case study of harm-based restrictions in democracies with weak rule of law versus strong institutional constraints.',
    'If capture occurs, the constraint becomes a snare for critics while maintaining tangled-rope coordination for marginalized groups—seat-specific classification emerges sharply. If institutional checks prevent capture, the constraint remains a tangled-rope stable across seats. The regime''s vulnerability (strong or weak courts, independent media, organized civil society) is the structural determinant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_risk, empirical, 'State institutional quality determines whether the constraint''s gatekeeping authority protects marginalized groups or becomes a tool for suppressing opposition.').

omega_variable(
    kernel_resolution_foreclosure,
    'Does the harm-limited reading logically foreclose the absolutist reading within a single institutional framework?',
    'Formal analysis: if courts adopt harm doctrine, can they simultaneously maintain Brandenburg''s imminent-lawless-action standard as the governing rule? If the standards logically conflict, the readings foreclose each other (only one can be law). If they can coexist (Brandenburg for one category, harm for another), they coexist rather than foreclose.',
    'If foreclosure is real, the absolutist reading is not live—it has been formally displaced and any lingering legal arguments for it are residual traditionalism, not competing frameworks. If coexistence is possible, the kernel remains contested and both readings remain live legal positions. This determines the reading_relations classification (forecloses vs. coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_resolution_foreclosure, conceptual, 'Whether the harm-limited and absolutist readings logically conflict or can coexist within constitutional doctrine.').

omega_variable(
    speech_as_subordination_empirical_grounding,
    'Does coordinated hate speech and harassment actually function as a subordination mechanism that forecloses equal participation, or is subordination a separate institutional phenomenon and speech a separate phenomenon that can be conflated rhetorically but not structurally?',
    'Empirical research on speech effects: controlled studies of the impact of coordinated hate campaigns on targeted group members'' willingness to participate in public discourse, political activism, and civic engagement. Comparative case study of marginalized communities'' participation before/after hate speech intensification cycles.',
    'If subordination is empirically real (speech + institutional structures combine to foreclose participation), the harm-limited reading''s axiom (equality includes protection from speech-subordination) is grounded and the constraint''s justification is strong. If speech is epiphenomenal to structural subordination (institutions are the real mechanism and speech is just one expression), the reading''s distinctiveness collapses and it converges toward balancing or categorical suppression doctrine without the special dignity claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_as_subordination_empirical_grounding, empirical, 'The empirical reality of speech-as-subordination determines whether the harm-limited reading addresses a distinct phenomenon or conflates separate processes.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of critics and absolutists (0.71) structural (external barriers to advocacy and legal obstacles to challenge) or internalized (self-censorship, identity-lock chilling effects, internalized fear)?',
    'Post-exit trajectory analysis: survey speakers who have modified or ceased their speech; separate those who faced legal consequences (structural suppression) from those who experienced no legal action but self-censored (internalized suppression). Track whether suppression persists after legal barriers are hypothetically removed.',
    'If suppression is mostly structural, it can be addressed by legal reform (narrowing harm doctrine, stronger protections for critics). If suppression is mostly internalized, legal reform alone will not restore the ex ante speech freedom—the constraint has created lasting inhibition that persists even after formal protection is restored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the constraint''s suppressive effect is structural barrier or internalized chilling effect determines the remedy''s pathway.').

omega_variable(
    kernel_contest_closure_timeline,
    'Will the harm-limited reading become the new governing principle, the absolutist reading persist as a competing pole, or will a synthesis (balancing doctrine incorporating harm categories but not absolute bars) emerge as the stable equilibrium?',
    'Longitudinal analysis of Supreme Court and international high court decisions over 20+ years: which reading captures the most jurisdiction? Do alternative readings persist as live positions or become residual traditionalism? Do synthesis positions emerge that blur the distinctions?',
    'If harm-limited reading becomes governing, the kernel moves toward resolution and absolutism becomes heritage rather than live jurisprudence—the constraint narrows from contested to settled, extraction from absolutists becomes permanent. If absolutism persists, kernel remains contested and the constraint''s legitimacy remains provisional. If synthesis emerges, the constraint''s gatekeeping might become more constrained (limiting harm categories in exchange for absolutist concession that some harms justify limits).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_closure_timeline, conceptual, 'Long-term kernel resolution or persistent contestation determines the constraint''s final institutional form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__harm_limited_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__harm_limited_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__harm_limited_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__harm_limited_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(spee_tr_t25, speech_protection_boundary__harm_limited_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__harm_limited_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__harm_limited_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__harm_limited_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__harm_limited_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__harm_limited_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(spee_be_t25, speech_protection_boundary__harm_limited_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__harm_limited_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__harm_limited_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__harm_limited_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__harm_limited_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__harm_limited_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(spee_su_t25, speech_protection_boundary__harm_limited_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__harm_limited_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(spee_grid_01, speech_protection_boundary__harm_limited_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(spee_grid_02, speech_protection_boundary__harm_limited_reading, accessibility_collapse(class), 40, 0.72).
narrative_ontology:measurement(spee_grid_03, speech_protection_boundary__harm_limited_reading, accessibility_collapse(individual), 0, 0.42).
narrative_ontology:measurement(spee_grid_04, speech_protection_boundary__harm_limited_reading, accessibility_collapse(individual), 40, 0.52).
narrative_ontology:measurement(spee_grid_05, speech_protection_boundary__harm_limited_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(spee_grid_06, speech_protection_boundary__harm_limited_reading, accessibility_collapse(organizational), 40, 0.63).
narrative_ontology:measurement(spee_grid_07, speech_protection_boundary__harm_limited_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(spee_grid_08, speech_protection_boundary__harm_limited_reading, accessibility_collapse(structural), 40, 0.58).
narrative_ontology:measurement(spee_grid_09, speech_protection_boundary__harm_limited_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(spee_grid_10, speech_protection_boundary__harm_limited_reading, resistance(class), 40, 0.68).
narrative_ontology:measurement(spee_grid_11, speech_protection_boundary__harm_limited_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(spee_grid_12, speech_protection_boundary__harm_limited_reading, resistance(individual), 40, 0.58).
narrative_ontology:measurement(spee_grid_13, speech_protection_boundary__harm_limited_reading, resistance(organizational), 0, 0.75).
narrative_ontology:measurement(spee_grid_14, speech_protection_boundary__harm_limited_reading, resistance(organizational), 40, 0.72).
narrative_ontology:measurement(spee_grid_15, speech_protection_boundary__harm_limited_reading, resistance(structural), 0, 0.73).
narrative_ontology:measurement(spee_grid_16, speech_protection_boundary__harm_limited_reading, resistance(structural), 40, 0.73).
narrative_ontology:measurement(spee_grid_17, speech_protection_boundary__harm_limited_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(spee_grid_18, speech_protection_boundary__harm_limited_reading, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(spee_grid_19, speech_protection_boundary__harm_limited_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(spee_grid_20, speech_protection_boundary__harm_limited_reading, stakes_inflation(individual), 40, 0.48).
narrative_ontology:measurement(spee_grid_21, speech_protection_boundary__harm_limited_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(spee_grid_22, speech_protection_boundary__harm_limited_reading, stakes_inflation(organizational), 40, 0.64).
narrative_ontology:measurement(spee_grid_23, speech_protection_boundary__harm_limited_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(spee_grid_24, speech_protection_boundary__harm_limited_reading, stakes_inflation(structural), 40, 0.42).
narrative_ontology:measurement(spee_grid_25, speech_protection_boundary__harm_limited_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(spee_grid_26, speech_protection_boundary__harm_limited_reading, suppression(class), 40, 0.75).
narrative_ontology:measurement(spee_grid_27, speech_protection_boundary__harm_limited_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(spee_grid_28, speech_protection_boundary__harm_limited_reading, suppression(individual), 40, 0.52).
narrative_ontology:measurement(spee_grid_29, speech_protection_boundary__harm_limited_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(spee_grid_30, speech_protection_boundary__harm_limited_reading, suppression(organizational), 40, 0.62).
narrative_ontology:measurement(spee_grid_31, speech_protection_boundary__harm_limited_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(spee_grid_32, speech_protection_boundary__harm_limited_reading, suppression(structural), 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three structurally distinct constraints, one per reading. This constraint (harm_limited_reading) models the doctrine that speech protection is conditional on absence of significant harm to dignity and equality. The absolutist_reading models the Brandenburg standard (near-absolute protection). The balancing_reading models case-by-case weighing. All three share the referent (standing speech-protection doctrine) but author different ε values because the harm thresholds and justifications differ. This is not observable-dependence: it is genuine alternative constraint structures that compete for institutional adoption. The three are linked via network.affects_constraints because adoption of one reading affects the viability and pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
