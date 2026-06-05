% ============================================================================
% CONSTRAINT STORY: professional_licensing_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_licensing_barriers, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: professional_licensing_barriers
 *   human_readable: Professional Licensing Barriers as Asymmetric Extraction
 *   domain: labor/regulation/professional_gatekeeping
 *
 * SUMMARY:
 *   Professional licensing creates a structural tension between legitimate
 *   consumer protection (ensuring practitioners meet quality standards) and
 *   systematic restriction of entry that maintains incumbent wage premiums
 *   and controls labor supply. This constraint exhibits all characteristics
 *   of asymmetric coordination-extraction (Tangled Rope): genuine
 *   coordination benefits exist (credential verification, quality assurance,
 *   consumer trust), but the coordination function is inseparable from the
 *   extraction mechanism (supply restriction, high barriers to entry,
 *   incumbent protection). The constraint is enforced through professional
 *   associations that control licensing boards, examination standards, and
 *   reciprocity rules. Theater is moderate (0.48) because much of the
 *   licensing requirement is genuine quality assurance rather than pure
 *   performative ritual, but substantial portions exist primarily to restrict
 *   supply. Extractiveness has increased over 20 years as barriers have
 *   accumulated (examination complexity, education requirements, fees,
 *   credential evaluation) without proportional quality improvement. The
 *   aspiring practitioner and consumer perspectives reveal the constraint as
 *   a Snare: powerless agents face structural suppression and zero exit
 *   options. The professional association sees coordination and experiences
 *   themselves as quality-guardians. The analytical observer sees the dual
 *   function: genuine coordination inseparably bound to asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - Incumbent Professionals: Primary beneficiaries (institutional/arbitrage) — capture wage premiums, reduced competition, and market control through licensing restrictions
 *   - Aspiring Practitioners: Primary victims (powerless/trapped) — face high barriers to entry, examination costs, time requirements, credential recognition delays with no alternative pathways
 *   - Professional Associations: Institutional controllers (organized/arbitrage) — maintain licensing board authority, set standards, coordinate credential verification, and enforce supply restriction
 *   - Foreign-Trained Professionals: Secondary victims (moderate/constrained) — possess credentials but face recognition barriers, reciprocity limitations, and additional costs
 *   - Consumers: Diffuse victims (powerless/trapped) — face reduced access, higher costs due to artificial scarcity, geographic unavailability, innovation delays
 *   - Regulatory Reform Coalition: Emerging organized agents (organized/mobile) — building interstate compacts, mutual recognition agreements, credential portability systems with sunset toward simplified verification
 *   - Licensing Boards: Enforcement apparatus (institutional/arbitrage) — administer examinations, verify credentials, enforce restrictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_licensing_barriers, 0.58).
domain_priors:suppression_score(professional_licensing_barriers, 0.65).
domain_priors:theater_ratio(professional_licensing_barriers, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_licensing_barriers, extractiveness, 0.58).
narrative_ontology:constraint_metric(professional_licensing_barriers, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(professional_licensing_barriers, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_licensing_barriers, tangled_rope).
narrative_ontology:human_readable(professional_licensing_barriers, "Professional Licensing Barriers as Asymmetric Extraction").
narrative_ontology:topic_domain(professional_licensing_barriers, "labor/regulation/professional_gatekeeping").

domain_priors:requires_active_enforcement(professional_licensing_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_licensing_barriers, incumbent_professionals).
narrative_ontology:constraint_beneficiary(professional_licensing_barriers, licensing_boards).
narrative_ontology:constraint_victim(professional_licensing_barriers, aspiring_practitioners).
narrative_ontology:constraint_victim(professional_licensing_barriers, underrepresented_populations).
narrative_ontology:constraint_victim(professional_licensing_barriers, consumer_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING PRACTITIONER (SNARE) — Faces high barriers to entry: examination costs, education requirements, apprenticeship hours, licensing fees, and credential recognition delays. Cannot exit the constraint without abandoning career aspirations. Suppression is structural: alternative credentialing pathways are actively blocked by licensing boards. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(professional_licensing_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FOREIGN-TRAINED PROFESSIONAL (TANGLED ROPE) — Possesses credentials but faces recognition barriers, credential evaluation costs, additional examination requirements, and reciprocity limitations. Has genuine coordination benefits (consumer protection through credential verification) alongside asymmetric extraction (substantial additional costs and delays compared to domestic applicants). Constrained exit: can practice in original country or apply for difficult reciprocal licensing, but both paths carry high costs.
constraint_indexing:constraint_classification(professional_licensing_barriers, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT PROFESSIONAL (ROPE) — Benefits from licensing restrictions that reduce competition and support wage premiums. Experiences the constraint as coordination: licensing ensures quality standards and consumer trust. Net beneficiary with arbitrage options (can relocate, change specialties, or exit the profession entirely). Extraction flows toward this agent.
constraint_indexing:constraint_classification(professional_licensing_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL ASSOCIATION (ROPE) — Maintains licensing board control, sets examination standards, coordinates credential verification, and enforces reciprocity rules. Genuinely coordinates quality and consumer protection (coordination function) but uses this authority to restrict supply and maintain incumbent wage premiums (extraction mechanism). Organized power with arbitrage options (can negotiate regulatory exemptions, adjust standards, influence state legislation). Coordination and extraction are structurally inseparable.
constraint_indexing:constraint_classification(professional_licensing_barriers, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (interstate compact systems, mutual recognition agreements, credential transparency initiatives) see licensing barriers as a temporary coordination problem with a sunset: national credential portability, competency-based assessment, and reciprocal recognition are reducing friction. Low effective extraction because organized agents have agency and see an exit path toward simplified verification systems.
constraint_indexing:constraint_classification(professional_licensing_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSUMER (ACCESS BARRIER) (SNARE) — Faces reduced access to services due to supply restrictions (fewer licensed practitioners), higher costs due to artificial scarcity and wage premiums, geographic unavailability in underserved areas, and delayed access to innovation. Cannot exit the constraint — consumers depend on licensed professionals and have no alternative verification mechanism. Suppression is structural and invisible: barriers appear as 'quality assurance' rather than rationing.
constraint_indexing:constraint_classification(professional_licensing_barriers, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, licensing serves dual functions: genuine consumer protection (coordination benefit) through credential verification and quality standards, but also systematic restriction of supply and restriction of entry (asymmetric extraction) that maintains incumbent wage premiums. The constraint is not pure extraction (many licensing rules genuinely improve quality) nor pure coordination (many rules exist to restrict supply). The perspectival gap reveals that professional associations conflate coordination and extraction in their rationale.
constraint_indexing:constraint_classification(professional_licensing_barriers, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_licensing_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_licensing_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_licensing_barriers, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_licensing_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(professional_licensing_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting substantial but not maximal extraction. The value reflects that licensing serves genuine coordination functions (quality assurance, consumer trust, credential verification) alongside extraction mechanisms (supply restriction, high barriers, incumbent protection). The extractiveness trajectory shows accumulation: as examination complexity increases and education requirements expand without proportional quality improvement, the extraction component rises relative to coordination. If barriers were pure supply restriction (no coordination function), extractiveness would approach 0.75-0.85 (snare range); the moderate value reflects genuine coordination value. Suppression (0.65): Moderately high. Structural barriers include examination costs ($300-$3,000+), education/apprenticeship hour requirements (1,000-4,000+ hours depending on profession), credential evaluation costs ($100-$500+), reciprocity limitations (state-by-state requirements), and publication bias against alternative credentialing. But suppression is not total — some jurisdictions have reciprocal agreements, and credential-blind alternative pathways exist in some fields. Theater ratio (0.48): Below the piton threshold (0.70) because much of the licensing requirement is genuine quality assurance rather than pure ritual. Examination content correlates with practitioner competency in many cases. But substantial portions of requirements (specific education hour counts, degree type requirements, apprenticeship duration) are performative proxies for competency rather than competency tests themselves.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival heterogeneity. The incumbent professional sees coordination and fair market incentives (Rope perspective). The aspiring practitioner sees an immovable barrier with zero exit options (Snare perspective). The professional association sees quality assurance and consumer protection (Rope perspective oriented toward their agency). The regulatory reform coalition sees a temporary problem with a sunset (Scaffold perspective). The consumer sees restricted access and artificial scarcity (Snare perspective). The analytical observer sees genuine coordination inseparably bound to extraction (Tangled Rope perspective). The perspectival gap does not resolve by choosing one 'correct' view — it reveals that the constraint's structure is genuinely hybrid. Beneficiaries and beneficiary-adjacent actors (incumbent professionals, associations) see rope-like coordination. Victims (aspiring practitioners, consumers, foreign-trained professionals) see snare-like extraction. The analytical observer sees both simultaneously, which is the defining feature of Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derived from beneficiary/victim declarations and exit options. Incumbent professionals are beneficiaries with arbitrage exit options (can relocate, change specialties, exit profession) — low d value (~0.15), negative experienced extraction (they benefit from the constraint). Professional associations are beneficiaries with arbitrage options — low d value (~0.10-0.20), institutional net benefit. Aspiring practitioners are victims with trapped exit options (cannot practice without licensing, cannot exit without abandoning career aspiration) — high d value (~0.92), maximum experienced extraction. Foreign-trained professionals are victims with constrained exit (can practice elsewhere or in original country, but at substantial cost) — moderate-high d value (~0.75), significant experienced extraction. Consumers are victims with trapped exit (depend on licensed professionals, no alternative verification) — high d value (~0.90), substantial experienced extraction. The regulatory reform coalition are organized agents with mobile exit (can build alternative systems, negotiate exemptions, influence legislation) — moderate d value (~0.45-0.55), moderate experienced extraction. The directionality structure confirms Tangled Rope: beneficiaries (low d, negative chi) and victims (high d, high chi) are structurally inseparable within the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by clarifying that Tangled Rope is the correct classification because professional licensing genuinely exhibits both coordination and extraction functions, and neither can be removed without removing the other. If the constraint were pure extraction (Snare), one could simply eliminate licensing and consumers would be safer (no coordination loss). But consumer protection through credential verification IS a real coordination benefit — eliminating licensing would create information asymmetry in practitioner quality. If the constraint were pure coordination (Rope), there would be no asymmetric extraction — but the evidence shows that barrier height far exceeds what is necessary for quality assurance alone, and incumbent wage premiums exist partly due to artificial scarcity. The classification refuses the false choice between 'all extraction disguised as coordination' and 'all coordination with some extraction overhead.' The dual function is structural. The mandatrophy is resolved by accepting that professional licensing is a genuine hybrid mechanism with substantial genuine coordination value AND substantial asymmetric extraction, and the policy question is not 'is it extraction?' but 'what ratio of coordination to extraction is justified and how do we shift it toward pure coordination without losing quality assurance?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_protection_vs_supply_restriction,
    'What fraction of licensing requirements genuinely protect consumer quality versus merely restrict supply?',
    'Cross-national comparative analysis: jurisdictions with lower barriers but equivalent quality outcomes; experimental credential-blind consumer satisfaction studies; correlation between barrier height and actual consumer protection outcomes',
    'If majority protective: higher coordination function, lower pure extraction, classification shifts toward Rope. If majority supply-restriction: lower coordination function, higher pure extraction, classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_protection_vs_supply_restriction, empirical, 'Fraction of licensing requirements that protect quality versus restrict supply').

omega_variable(
    credential_portability_timeline,
    'How quickly will interstate reciprocity, national credentials, and mutual recognition systems reduce licensing barriers?',
    'Historical tracking of APLE (American Professional License Exchange), occupational licensing reform legislation, compact adoption rates, and credential portability timelines in leading jurisdictions',
    'If sunset < 10 years: scaffold perspective confirmed, constraint is degrading. If sunset > 25 years: sunset is aspirational, classification remains tangled_rope or snare. If no sunset materializes: reform coalition is theater, classification shifts toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_portability_timeline, empirical, 'Timeline for significant credential portability and barrier reduction').

omega_variable(
    competency_verification_alternative,
    'Can competency-based assessment (skills testing, portfolio review, apprenticeship records) replace education-hours-based licensing without reducing consumer protection?',
    'Pilot programs with competency-based pathways; comparison of consumer outcomes and safety records between hours-based and competency-based jurisdictions',
    'If viable: alternative verification exists, extraction mechanism loses force, classification shifts toward Rope or Scaffold. If not viable: suppression is justified by actual consumer protection needs, extraction remains necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competency_verification_alternative, empirical, 'Viability of competency-based assessment as licensing alternative').

omega_variable(
    incumbent_wage_premium_extraction,
    'What fraction of incumbent wage premiums in licensed professions derives from genuine skill scarcity versus artificial supply restriction?',
    'Econometric analysis: wage differential between licensed and unlicensed comparable practitioners; wage changes in jurisdictions that relax licensing; international wage comparison for same profession with different licensing stringency',
    'If majority skill scarcity: extraction component is smaller than apparent, classification skews toward Rope. If majority supply restriction: extraction is severe, classification confirms Snare or high-chi Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_wage_premium_extraction, empirical, 'Fraction of incumbent wage premiums from scarcity versus restriction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_licensing_barriers, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plb_tr_t0, professional_licensing_barriers, theater_ratio, 0, 0.35).
narrative_ontology:measurement(plb_tr_t10, professional_licensing_barriers, theater_ratio, 10, 0.42).
narrative_ontology:measurement(plb_tr_t20, professional_licensing_barriers, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(plb_be_t0, professional_licensing_barriers, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(plb_be_t10, professional_licensing_barriers, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(plb_be_t20, professional_licensing_barriers, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_licensing_barriers, information_standard).
narrative_ontology:affects_constraint(professional_licensing_barriers, occupational_wage_inequality).
narrative_ontology:affects_constraint(professional_licensing_barriers, credential_recognition_barriers).

% DUAL FORMULATION NOTE:
% Professional licensing barriers decompose into separate constraints per the ε-invariance principle. The licensing-as-quality-assurance constraint (ε ≈ 0.15, information coordination function) is distinct from the licensing-as-supply-restriction constraint (ε ≈ 0.72, incumbent extraction mechanism). The unified story models the inseparability of these functions in practice; if they could be decomposed empirically, two separate stories would be warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
