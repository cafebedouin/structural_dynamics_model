% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements as Graduated Access Filter
 *   domain: labor/regulatory
 *
 * SUMMARY:
 *   Statutory credential requirements in licensed fields (medicine, law,
 *   electricianry, real estate, cosmetology, etc.) are justified as
 *   consumer-protection mechanisms preventing harm from incompetent
 *   practitioners. This reading frames the same requirements as a snare: a
 *   structural mechanism that creates tiered market access where differential
 *   barriers sort labor-market entry by class and prior resource access.
 *   Marginalized workers without credential-acquisition resources (time,
 *   capital, geographic mobility, freedom from disqualifying records) are
 *   structurally excluded from offering labor in these fields, while
 *   incumbents and those with resource access benefit from reduced
 *   competition and can maintain higher compensation. The constraint is one
 *   reading of the 'licensing_statute_mandate' kernel, alongside competing
 *   readings (public_safety_coordination, rent_seeking_suppression) that
 *   emphasize different aspects of the same rule.
 *
 * KEY AGENTS:
 *   - Credentialed incumbent practitioners: set and administer credential standards through professional boards; benefit from reduced competition and higher compensation
 *   - Marginalized workers without credential resources: structurally excluded from entry; cannot afford exam costs, apprenticeship time, or licensing fees; bear the constraint as trapped victims
 *   - Lower-income aspirants: face constrained but surmountable barriers; must choose between high financial cost of credentials or accepting lower-wage alternatives
 *   - Professional licensing boards: govern standard-setting and exam approval; dominated by incumbents; have institutional incentive to maintain barriers
 *   - Consumers of services: benefit from baseline competence signal; bear costs indirectly through higher service prices from restricted supply
 *   - State revenue apparatus: collects licensing fees; has budgetary incentive to maintain licensing regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.81).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor/regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '19026110-c6cd-4882-8794-c91babb4d1aa').
narrative_ontology:cs_kernel_codification('19026110-c6cd-4882-8794-c91babb4d1aa', formalized).
narrative_ontology:cs_authority_grounding('19026110-c6cd-4882-8794-c91babb4d1aa', extraction).
narrative_ontology:cs_interpretation_layer_present('19026110-c6cd-4882-8794-c91babb4d1aa').
narrative_ontology:cs_reading_relation('19026110-c6cd-4882-8794-c91babb4d1aa', licensing_statute_mandate__public_safety_coordination, influences).
narrative_ontology:cs_reading_relation('19026110-c6cd-4882-8794-c91babb4d1aa', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('19026110-c6cd-4882-8794-c91babb4d1aa', foundational, credential_barriers_sort_by_class_not_competence).
narrative_ontology:cs_axiom_status(credential_barriers_sort_by_class_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('19026110-c6cd-4882-8794-c91babb4d1aa', credential_barriers_sort_by_class_not_competence, empirically_contingent).
narrative_ontology:cs_axiom('19026110-c6cd-4882-8794-c91babb4d1aa', foundational, incumbent_extraction_exceeds_safety_requirement).
narrative_ontology:cs_axiom_status(incumbent_extraction_exceeds_safety_requirement, holdable).
narrative_ontology:cs_axiom_grounding('19026110-c6cd-4882-8794-c91babb4d1aa', incumbent_extraction_exceeds_safety_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('19026110-c6cd-4882-8794-c91babb4d1aa', competence_verification_framework).
narrative_ontology:cs_drift_state('19026110-c6cd-4882-8794-c91babb4d1aa', contemporary_regulatory_capture_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('19026110-c6cd-4882-8794-c91babb4d1aa', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_credential_resources).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, lower_income_aspirants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, lower_wage_alternate_service_providers).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, state_revenue_apparatus).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credential_acquisition_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licensed practitioners in established disciplines (law, medicine, electrician, real estate, cosmetology, nursing, accounting, etc.). They sit on or heavily influence state licensing boards that set credential standards, exam difficulty, approval of training providers, licensing fees, and disciplinary processes. They benefit directly from the credential requirement by excluding lower-cost competitors and maintaining higher compensation. They frame the requirement as essential consumer protection and defend high barriers as necessary for safety. Their exit options are high (arbitrage): they can work in adjacent unlicensed fields, relocate to lower-barrier states, or retrain if forced, but they have strong incentive to prevent such exits since their credential premium depends on high barriers.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, beneficiary).

% Workers attempting entry into licensed occupations but lacking resources to acquire credentials: individuals from low-income backgrounds, those with limited formal education, workers with criminal records (many fields disqualify), immigrants with language barriers or foreign credentials not recognized, workers in rural areas distant from training providers, parents unable to leave full-time work for unpaid apprenticeships, and those already carrying high debt loads. For them, the credential barrier is often insurmountable. They cannot offer their labor in licensed fields even if they have capability equivalent to credentialed practitioners. Their exit options are trapped: they cannot exit by paying the cost (they lack the capital) and cannot exit by going unlicensed (unlicensed work in regulated fields is prohibited). They absorb the constraint as structurally exclusionary.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_credential_resources, payer,
    powerless, biographical, trapped, national).

% Workers with some capacity to acquire credentials but facing cumulative barriers: cost of exams ($200–$1,000+ per attempt), licensing fees ($100–$500+), required training/apprenticeship (often unpaid or low-wage for 6 months to 3 years), opportunity cost of lost wages during training, debt service on prior education, geographic mobility to reach training providers, and time demands on workers with family obligations. The barriers are technically surmountable but require substantial sacrifice. They face a constrained choice: pursue the credential at high financial and opportunity cost, or accept lower-wage unlicensed work, underemployment, or different fields. Many either delay credential acquisition indefinitely or abandon the attempt.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, lower_income_aspirants, payer,
    moderate, biographical, constrained, national).

% State licensing boards that administer credential standards, exams, and disciplinary processes. Nominally independent regulatory bodies, they are staffed and governed by incumbent practitioners from the regulated field (often 70–90% of board members are licensed practitioners). This governance structure creates structural incentive alignment with incumbent interests: boards set standards that advantage incumbent-aligned training pathways, approve exams that incumbent-affiliated prep companies help pass, and maintain licensing fees that benefit the board's institutional budget. They have institutional power to set barriers and actively resist attempts to reduce them as threats to 'standards' (framing extraction as safety).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, incumbent_professional_boards, agenda_setter,
    institutional, generational, analytical, national).

% End consumers of services in licensed fields (patients seeing doctors/dentists, clients hiring lawyers, customers hiring electricians/plumbers, etc.). They benefit from a baseline competence signal: the credential provides assurance that practitioners meet minimum technical standards, reducing information asymmetry and harm risk from incompetence. This is a real benefit. They also bear an indirect cost: higher service prices passed through from licensed practitioners' market power and reduced supply competition. Consumers have constrained exit: they must use licensed practitioners if services are regulated, and they have minimal voice in standard-setting. Their beneficiary status is genuine but passive.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, beneficiary,
    moderate, biographical, constrained, national).

% Workers who provide equivalent or partial services in unlicensed adjacent niches: unlicensed handymen/contractors, alternative and complementary health practitioners, paralegal and legal-tech services, unaccredited counselors/life coaches, unlicensed childcare. The credential requirement in the adjacent licensed field reduces direct competition for their unlicensed services (price-sensitive consumers cannot hire licensed practitioners, so they turn to unlicensed alternatives). This protection allows them to charge higher prices or capture market share they would lose to licensed competitors if barriers fell. They are structurally protected from direct competition, which is a benefit, though this benefit is passive (they do not set the standard and would lose it if barriers fell).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, lower_wage_alternate_service_providers, beneficiary,
    moderate, biographical, constrained, national).

% State governments and departments that collect licensing fees and regulatory fines. Annual licensing revenues can reach tens of millions statewide (thousands of practitioners at $100–$500 per renewal per year). These revenues fund regulatory budgets, and in some states become general revenue. The credential requirement generates steady funding. While not the primary driver of the constraint, state budgetary dependency on licensing revenue weakly reinforces the barrier maintenance and creates political friction against reduction (lost revenue).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_revenue_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Federal and state consumer-protection and antitrust authorities (FTC, state attorneys general, occupational licensing reform initiatives). They investigate whether credential requirements reduce competition without corresponding consumer benefit, whether barriers are set unnecessarily high, and whether the governance structure (incumbent-dominated boards) creates conflicts of interest. They have authority to recommend deregulation, challenge specific barriers, or require process reforms. Their enforcement is sporadic and faces political opposition from incumbent practitioners and state revenue interests, but they represent the possibility of external pressure on the constraint.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumer_protection_regulators, observer,
    institutional, generational, analytical, national).

% Training schools, apprenticeship programs, exam prep companies, certification bodies, and online education platforms that provide credential-acquisition services. They benefit from mandatory credentialing: steady and growing demand for training, exam preparation, and testing administration. They have financial incentive to maintain and expand credential requirements (more credentialing = more customer demand). They often align with incumbent practitioners in professional associations and regulatory processes, reinforcing barrier maintenance and sometimes even barrier expansion (more continuing education requirements = more repeat customers).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credential_acquisition_providers, beneficiary,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the practitioner-competence information-asymmetry problem: consumers cannot easily assess whether practitioners possess minimum technical knowledge in high-stakes fields (medical care, legal services, construction safety, electrical systems). A standardized credential reduces information-search cost and provides a baseline safety signal.
% TRANSFER_FUNCTION: Moves economic rent (wage and price premium) from lower-income aspiring workers and price-sensitive consumers toward credentialed incumbents and credential-acquisition providers. The transfer mechanisms: (1) wage suppression of potential low-cost competitors through barrier-to-entry exclusion; (2) price elevation for end consumers accepting restricted supply; (3) credential-acquisition spending redirected to training providers and licensing bodies; (4) state licensing revenue; (5) reduced competition for unlicensed adjacent service providers. The core transfer is from those without credential resources to those with them or already credentialed.
% ABSENT_VOICES: Marginalized workers excluded from entry are not represented in standard-setting processes: they lack standing on professional boards, cannot afford legal advocacy, have high labor-market turnover, and have limited collective organization (precarious conditions make organizing difficult). Consumers bear indirect costs but have no formal voice in credential-level decisions. The constraint is set entirely by credentialed incumbents and their aligned institutions (boards, training providers, state revenue apparatus). Those structurally excluded from the labor market have no seat at the table where the barrier height is decided.
% DISAPPEARANCE_RATIONALE: If the credential requirement vanished overnight in a licensed field, multiple rearrangements would follow: labor supply would expand sharply (lower-wage entrants without credentials could offer services), service prices would compress (competition would increase), incumbent practitioner wages/rents would fall (supply increased, price reduced), and the distribution of opportunity would shift dramatically from credentialed to less-credentialed workers. Consumers would get lower-cost services but with higher heterogeneity in quality/safety. This is not a stable world-unchanged outcome; the constraint actively sorts who can work in these fields and maintains incumbent advantage.
% FOUNDING_PROBLEM: Early practitioners in high-stakes fields (medicine, law, skilled trades, etc.) operated without minimum competence standards. Incompetent practitioners caused direct consumer harm (medical malpractice, faulty construction, negligent legal advice, etc.). Unscrupulous or unskilled practitioners gained market share through deception or luck rather than demonstrated competence. Consumers had no reliable way to identify minimally competent practitioners. The founding problem is genuine and persistent: information asymmetry about practitioner competence creates real harm.
% FOUNDING_PROBLEM_CORROBORATION: Consumer-protection advocates, medical boards, construction safety authorities, and patient-advocacy groups affirm that the founding problem persists: medical errors still cause patient harm, faulty construction causes injury and property loss, negligent legal advice causes financial loss. However, labor economists, antitrust authorities, and occupational licensing reform advocates from outside the benefiting parties document that (1) current barrier levels exceed what consumer safety alone would require (measured via harm-rate comparison with lower-barrier adjacent fields and across jurisdictions with different barrier levels), (2) many of the current barriers correlate with class/resource access rather than competence or harm prevention, and (3) incumbent practitioners actively resist evidence-based barrier reduction that would maintain safety while expanding access. This testimony establishes the founding problem as live but the current barrier level as reflecting accumulated rent-seeking beyond what consumer safety requires.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers substantial economic rent from lower-income workers and consumers toward credentialed incumbents through labor-supply suppression and price elevation. The transfer is quantifiable: wage gaps between licensed and unlicensed adjacent workers, price premiums for licensed services, and opportunity cost for aspiring workers excluded from entry. Suppression is correspondingly high (0.81) because the constraint's persistence depends on actively excluding lower-cost labor through barrier maintenance (difficult exams, high licensing fees, approval processes controlled by incumbents, credential portability restrictions across jurisdictions, disqualification criteria). These barriers are not consequences of consumer demand but active institutional enforcement. Theater ratio (0.42) is moderate: some of the enforcement activity is genuine competence-verification (exam administration, continuing education requirements), but a growing share is devoted to barrier maintenance and exclusion rather than competence assurance. The measurement series shows extractiveness and suppression rising over the interval (0–30) and then plateauing, reflecting accumulating rent-seeking layered onto the constraint as incumbent control hardened and political economy shifted toward their favor. Theater ratio also rises, indicating growing performative activity around competence justification even as the exclusionary function dominates.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed incumbent seat and the professional board seat, the constraint appears as legitimate consumer protection: they authored it to maintain competence standards and frame all enforcement as safety-necessary. From the marginalized worker seat and lower-income aspirant seat, the same constraint is experienced as structural exclusion: the same barriers that supposedly ensure competence are applied asymmetrically to exclude them despite equivalent capability, and the barriers are explicitly designed (expensive exams, geographic inconvenience, time requirements) in ways that disadvantage their resource profile. From the state revenue seat, the constraint is a positive financial mechanism. From the consumer seat, the constraint brings genuine benefit (competence signal) but at a cost (higher prices) the consumer does not consent to or control. The engine will compute different effective extraction (χ) for each seat from the structural data (beneficiary/victim declarations, power atoms, exit options, spatial scope): beneficiary seats get low/negative χ (subsidy), payer seats get high χ (extraction), symmetric seats get mid-range χ. The perspectival gap is the structural asymmetry in this computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents and professional boards are the structural beneficiaries: they set the standard, benefit from reduced competition and higher compensation, face high exit costs only because they have already invested in credentials (arbitrage exit available to them — they can work in adjacent unlicensed fields if needed), and hold organized/institutional power. Their directionality (d) is near 0.0 (full beneficiary). Marginalized workers without credential resources are the structural victims: they cannot enter the field because barriers are prohibitive, they cannot exit to an alternative (trapped exit — the constraint is in fields with no good substitutes), they hold powerless power (no leverage in standard-setting), and the barrier distribution explicitly targets their resource profile (cost, time, geography, disqualification). Their directionality (d) approaches 1.0 (full target). Lower-income aspirants sit intermediate: they can technically access credentials but at high cost, so their exit is constrained rather than trapped; their directionality (d) is around 0.65–0.75 (primarily target, partially constrained). Consumers sit symmetric: genuine coordination benefit (safety signal) paired with diffuse indirect cost (higher prices), so directionality (d) is near 0.5. This directionality distribution is the ground for the snare classification: clear beneficiary (incumbents), clear victims (marginalized workers), active enforcement to maintain the exclusion, and significant extraction flowing upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing consumer harm from incompetent practitioners) is live: incompetence still causes harm, information asymmetry persists, and competence standards have genuine value. However, the constraint has accumulated extraction well beyond what consumer safety alone requires. This is the mandatrophy signature: the original purpose (consumer safety) persists and justifies the constraint's existence, but the constraint has been expanded and hardened to serve extractive functions (incumbent rent collection) that dominate its operation. The rising extractiveness (0.62 → 0.78) and theater ratio (0.25 → 0.42) over the interval show accumulated rent-seeking: once the constraint was in place, incumbents added barriers (higher exam difficulty, more continuing education requirements, geographic restrictions on reciprocal licensing, disqualification criteria) that do not improve consumer safety measurably but do exclude lower-cost competitors. The mandatrophy resolution is that the constraint must be decomposed: retain the genuine competence-verification function (which would justify lower barriers) and eliminate the rent-seeking expansions (which do not). This reading explicitly foregrounds the rent-seeking side of that decomposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_verification_vs_rent_extraction,
    'What fraction of the current credential-requirement barriers serve genuine consumer-safety competence verification, vs. pure rent extraction for incumbents?',
    'Comparative analysis: measure harm rates (medical error, construction failure, legal malpractice) in fields with high-barrier credentials vs. lower-barrier credentials vs. unlicensed adjacent services, controlling for practitioner experience; correlate barrier difficulty with measured harm reduction. If harm reduction plateaus below the current barrier level, the excess is extraction rather than safety.',
    'If a significant fraction (>40%) of barriers serve no competence function, the constraint reclassifies from a mixed tangled_rope (genuine coordination + extraction) toward pure snare (extraction only). If barriers and measured harm reduction align tightly (<20% excess), the constraint maintains Tangled Rope character despite asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_verification_vs_rent_extraction, empirical, 'Whether credential barriers are proportionate to consumer-safety needs or include extraction-only components.').

omega_variable(
    barrier_class_correlation_intentionality,
    'Are the observed class-correlated barriers (high cost, geographic inconvenience, time requirements, disqualification criteria) designed intentionally to sort by resource access, or are they coincidental consequences of other bureaucratic choices?',
    'Historical analysis: examine professional board meeting minutes, legislative testimony, and regulatory impact assessments from barrier-design periods. If incumbents explicitly discussed barrier height in terms of supply restriction or proposed specific barriers to target lower-income applicants, intentionality is established. If barriers were adopted without consideration of resource-access consequences, the class sorting may be structural-but-unintended.',
    'Intentional design strengthens the snare classification (knowing exclusion is extraction). Structural-but-unintended preserves snare (harm is harm regardless of intent) but affects mandatrophy analysis (less-willful capture = different remediation path).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(barrier_class_correlation_intentionality, empirical, 'Whether class-differentiated barriers reflect deliberate rent-seeking strategy or emergent structural effects.').

omega_variable(
    reading_contestation_stability,
    'Which reading (public_safety_coordination, rent_seeking_suppression, or graduated_access_filter) has the strongest institutional anchor? Which reading is most vulnerable to empirical falsification or political reversal?',
    'Political-economy analysis: identify which stakeholders have most power to enforce their reading via standard-setting, legislative amendment, or regulatory priority. Track shifts in professional board composition, state attorney-general priorities, and occupational-regulation reform momentum over the interval.',
    'If public_safety_coordination reading gains institutional dominance (reflected in regulatory language, board composition, legislative intent), the constraint may be reread as genuine rope rather than snare, and the rent-extraction components may be reformulated as ''side effects'' rather than primary function. If rent_seeking_suppression or graduated_access_filter readings gain dominance, barriers face political pressure for reduction or class-blind restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_stability, conceptual, 'Which reading of the kernel licensing_statute_mandate has institutional staying power.').

omega_variable(
    identity_lock_credentialed_practitioners,
    'For credentialed incumbent practitioners, how much of their opposition to credential-barrier reduction is rooted in genuine belief that high barriers ensure safety, vs. career/professional identity fusion with credential status, vs. economic rent protection?',
    'Qualitative research: survey and interview incumbent practitioners about their motivations for barrier maintenance. Observe their response to evidence that lower barriers don''t increase harm rates. Measure whether belief changes when economic incentives are held constant (e.g., licensing fee reduction to compensate for reduced wage premium).',
    'If identity fusion is substantial (practitioners experience barrier reduction as professional degradation even when economic incentives are addressed), the constraint has internalized suppression for this agent group and is more resistant to reform. If opposition is purely economic, regulatory restructuring to preserve income while opening barriers is more feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_credentialed_practitioners, empirical, 'Whether incumbent practitioners are identity-locked to the high-barrier regime or rationally defending economic rent.').

omega_variable(
    sibling_reading_empirical_divergence,
    'Can the three sibling readings (public_safety_coordination, rent_seeking_suppression, graduated_access_filter) be empirically distinguished, or do all three describe the same constraint and simply emphasize different metrics?',
    'Comparative classification: author full constraint stories for all three readings (same kernel, different readings); run each through the engine''s per-seat classification algorithm. If the three readings yield different constraint types at majority of seats, they are truly distinct readings. If they all compute to the same type at all seats, they are observationally equivalent framings and only differ in narrative emphasis.',
    'If empirically distinct, the three readings are genuine alternate kernel interpretations and should be maintained as three separate files linked via network.affects_constraints. If observationally equivalent, they are narrative variants of a single reading and should be collapsed into one constraint with commentary flagging the interpretive dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_divergence, conceptual, 'Whether the kernel''s three sibling readings are structurally distinct or narratively equivalent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__graduated_access_filter, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(lice_tr_t5, observed).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(lice_tr_t10, observed).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__graduated_access_filter, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(lice_tr_t15, observed).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(lice_tr_t20, observed).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__graduated_access_filter, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(lice_tr_t25, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(lice_tr_t30, observed).
narrative_ontology:measurement(lice_tr_t35, licensing_statute_mandate__graduated_access_filter, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(lice_tr_t35, projected).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(lice_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(lice_be_t5, observed).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(lice_be_t10, observed).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 15, 0.74).
narrative_ontology:measurement_basis(lice_be_t15, observed).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(lice_be_t20, observed).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(lice_be_t25, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(lice_be_t30, observed).
narrative_ontology:measurement(lice_be_t35, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(lice_be_t35, projected).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(lice_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(lice_su_t5, observed).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(lice_su_t10, observed).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(lice_su_t15, observed).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(lice_su_t20, observed).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(lice_su_t25, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(lice_su_t30, observed).
narrative_ontology:measurement(lice_su_t35, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 35, 0.81).
narrative_ontology:measurement_basis(lice_su_t35, projected).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(lice_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, resource_allocation).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__graduated_access_filter, 0.12).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member kernel family around the licensing_statute_mandate kernel. All three stories describe the same statutory rule (credential requirement in licensed fields) but instantiate different readings with different structural interpretations. The graduated_access_filter reading emphasizes how the same barriers extract differentially based on class and prior resource access. This reading influences both sibling readings because empirical evidence of class-correlated barriers creates structural pressure on the public_safety_coordination reading to explain why safety would require class sorting, and it weakens the rent_seeking_suppression reading if barriers can be shown to have partial genuine safety function. Link all three via network.affects_constraints; see commentary.kernel_context for reading contestation details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
