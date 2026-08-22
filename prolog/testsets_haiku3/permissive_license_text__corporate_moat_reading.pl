% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_corporate_moat, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text: Corporate Moat Reading
 *   domain: software/intellectual property/technology governance
 *
 * SUMMARY:
 *   A permissive open-source license (MIT, Apache 2.0, BSD) permits any use,
 *   including proprietary product development, without requiring source
 *   sharing, profit sharing, or even meaningful attribution. This reading
 *   interprets that permission as enabling systematic uncompensated
 *   extraction: enterprise corporations identify valuable open-source
 *   components, incorporate them into proprietary products, and capture all
 *   downstream revenue while the original maintainer receives no compensation
 *   and no access to derivative work. The constraint is claimed as a snare
 *   because the persistence of the arrangement depends on suppressing the
 *   visibility of extraction (framing it as 'freedom' and 'reuse') and on the
 *   maintainers' identity-lock that prevents them from switching to
 *   reciprocal licensing after the extraction becomes apparent.
 *
 * KEY AGENTS:
 *   - independent_open_source_maintainers: powerless, identity-locked (maintainer identity fused with open-source philosophy); primary victims
 *   - enterprise_product_corporations: institutional power, beneficiaries (capture uncompensated labor as product feature)
 *   - small_companies_competing_with_incumbents: powerful but constrained (same code freely available to enterprises with better distribution)
 *   - permissive_license_advocates: moderate power, believe in unrestricted reuse (ideological beneficiary of the constraint's legitimacy)
 *   - copyleft_alternative_advocates: excluded stakeholders (would redesign licensing with reciprocity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.71).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text: Corporate Moat Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software/intellectual property/technology governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'da13d512-20db-4827-b412-f765df105ed6').
narrative_ontology:cs_kernel_codification('da13d512-20db-4827-b412-f765df105ed6', fixed_text).
narrative_ontology:cs_authority_grounding('da13d512-20db-4827-b412-f765df105ed6', expertise).
narrative_ontology:cs_interpretation_layer_present('da13d512-20db-4827-b412-f765df105ed6').
narrative_ontology:cs_reading_relation('da13d512-20db-4827-b412-f765df105ed6', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('da13d512-20db-4827-b412-f765df105ed6', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('da13d512-20db-4827-b412-f765df105ed6', foundational, uncompensated_labor_transfer_structural).
narrative_ontology:cs_axiom_status(uncompensated_labor_transfer_structural, holdable).
narrative_ontology:cs_axiom_grounding('da13d512-20db-4827-b412-f765df105ed6', uncompensated_labor_transfer_structural, empirically_contingent).
narrative_ontology:cs_axiom('da13d512-20db-4827-b412-f765df105ed6', foundational, permissive_licensing_enables_corporate_extraction).
narrative_ontology:cs_axiom_status(permissive_licensing_enables_corporate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('da13d512-20db-4827-b412-f765df105ed6', permissive_licensing_enables_corporate_extraction, instrumental).
narrative_ontology:cs_reference_frame('da13d512-20db-4827-b412-f765df105ed6', permissive_license_as_coordination_solution).
narrative_ontology:cs_drift_state('da13d512-20db-4827-b412-f765df105ed6', contemporary_software_scale, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da13d512-20db-4827-b412-f765df105ed6', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_product_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, independent_open_source_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, small_companies_competing_with_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, downstream_proprietary_product_users).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, permissive_license_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and maintains open-source software under a permissive license (MIT, Apache 2.0, BSD). Contributes labor without compensation, motivated by belief in open-source ideals and community benefit. An enterprise corporation takes the code, builds a proprietary product on it, and captures all revenue while the maintainer receives no compensation, attribution, or derivative source access. The maintainer's identity is constituted through the open-source community and the belief that code sharing benefits everyone; the discovery that their work becomes a vehicle for extractive corporate products creates internal identity conflict.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, independent_open_source_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Scans open-source ecosystems for reusable components. Incorporates permissively-licensed code into proprietary products with no obligation to contribute back, share improvements, or credit the original authors beyond minimal legal attribution (often buried in a LICENSE file). Justifies the practice as efficient reuse and benefit from volunteer innovation. Collects all downstream revenue from products built on uncompensated labor.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_product_corporations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, enterprise_product_corporations, beneficiary).

% May also use the same permissively-licensed code to build products, but lack the distribution, brand, and market power to reach scale. Face incumbent corporations leveraging the same open-source components with superior go-to-market capacity, turning free code into a competitive disadvantage for smaller players who cannot match enterprise marketing and sales. The permissive license removes a potential leveling mechanism (reciprocal source-sharing requirements).
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, small_companies_competing_with_incumbents, payer,
    powerful, biographical, constrained, global).

% Get access to mature, tested software functionality baked into proprietary products at prices the vendor sets. They receive genuine value (the incorporated open-source innovation) without bearing its development cost, but they also lose visibility into derivative work and cannot participate in improvements to the codebase.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, downstream_proprietary_product_users, beneficiary,
    organized, biographical, mobile, global).

% Advocate for permissive licensing as maximizing freedom and adoption. They ideologically benefit from the constraint's legitimacy and institutional acceptance. Their position is that all uses—including proprietary extraction—are legitimate because 'freedom' includes the freedom to build proprietary software. This reading directly contradicts their endorsed narrative.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, permissive_license_advocates, beneficiary,
    moderate, biographical, constrained, global).

% Argue that reciprocal licensing (GPL, AGPL) is structurally necessary to prevent uncompensated extraction and to ensure derivative works remain in the commons. They would redesign the licensing regime to require source-sharing on proprietary products built on open-source code. They are effectively excluded from the decision to relax copyright terms in favor of permissive rather than reciprocal frameworks.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_alternative_advocates, excluded,
    moderate, biographical, constrained, global).

% Maintain the legal framework and certify licenses as OSI-compliant. They set the rules under which permissive relaxation is permissible. Their enforcement is primarily validation-gatekeeping: a license must conform to the OSI definition (no field-of-use restrictions, no derivative-work requirements beyond attribution). They do not directly benefit or lose from the extraction, but their architectural choices (which license templates they endorse and propagate) shape whether corporations can legally do what this constraint describes.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, software_licensing_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Studies the socioeconomic effects of permissive versus reciprocal licensing on innovation distribution, maintainer burnout, and corporate leverage. Measures outcomes and documents the structural asymmetries this reading describes.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, observational_seat_academic_research, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_product_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing removes legal friction to code reuse: any party can incorporate the software without negotiation, licensing complexity, or legal review. This lowers adoption barriers for beneficial software multiplication and compatibility work.
% TRANSFER_FUNCTION: Moves uncompensated labor from independent maintainers to enterprise corporations: the maintainer's code is incorporated into proprietary products from which the corporation captures revenue, while the maintainer receives no compensation, derivative-work access, or attribution beyond minimal legal boilerplate.
% ABSENT_VOICES: Copyleft advocates (who would argue for reciprocal-source requirements) are excluded from the licensing regime choice; their alternative frameworks are not selected when permissive relaxation is adopted. The maintainers who would object to their work being used as uncompensated input to proprietary products are often invisible to the corporations doing the extraction — the constraint's operation is deliberately obscured by the legal framing ('freedom to use') that licenses the extraction.
% DISAPPEARANCE_RATIONALE: If permissive licensing and its enforcement (allowing proprietary use without reciprocity) disappeared, the software economy would shift dramatically: corporations would either pay maintainers for code (or acquisition), adopt GPL or other reciprocal licenses to retain competitive advantage through shared improvements, or build proprietary alternatives. Open-source sustainability would change; maintainer compensation models would shift; corporate product strategy would reorganize around source-sharing or payment.
% FOUNDING_PROBLEM: Early software copyright and licensing were restrictive and complex: developers faced legal uncertainty, incompatible license stacks, and friction when combining code. Permissive licensing was designed to maximize compatibility and eliminate legal barriers to beneficial reuse.
% FOUNDING_PROBLEM_CORROBORATION: Permissive-license advocates and software licensing bodies attest the founding problem is still live, citing compatibility complexity in GPL-mixed codebases. Independent maintainers and copyleft advocates attest the founding problem has been solved (compatibility is now well-understood, GPL stacks are manageable) and that permissive relaxation now primarily enables corporate extraction without solving a coordination problem. Academic research documents the shift: early permissive licensing solved friction; sustained permissive-only regime enables asymmetric extraction.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the constraint systematically transfers uncompensated labor from maintainers to corporations: the maintainer bears all development cost, the corporation captures all revenue derivative from that labor. Suppression is high (0.71) because the constraint's operation depends on obscuring the extraction through legal/philosophical framing ('software freedom' = freedom to use without reciprocity) and on maintainers' identity-fusion that prevents exit. Theater is moderate (0.42) because the permissive-license advocates genuinely believe in the coordination story (maximizing freedom, reducing friction), but that story increasingly serves as cover for extraction as corporate products at scale consume maintainer labor uncompensated. Accessibility collapse is moderate (0.52) because alternatives exist (GPL, AGPL, other reciprocal licenses) and maintainers are aware of them, but switching after years of permissive-license reputation costs is identity-dissonant ('my whole project is built on freedom; switching to copyleft feels like betrayal'). Resistance is moderate (0.58) because the copyleft movement articulates objections and some maintainers switch, but the institutional momentum and the identity-cost of switching hold most maintainers in place. Measurement trajectory: early (t=0, t=3) shows low extractiveness when permissive licensing was novel and genuinely solved coordination friction; by t=12-25 the constraint operates primarily as extraction apparatus as enterprises scale product lines on uncompensated code.
 *
 * PERSPECTIVAL GAP:
 *   The corporate beneficiary seat experiences this as coordination ('we solved distribution and reuse friction by adopting permissive licenses') and sees no extractive intent. The maintainer seat experiences this as theft ('I gave labor away under a framework that made me believe it was for the common good; I now see my work funds proprietary products I cannot participate in and receive no compensation for'). The licensing-body seat (agenda-setter but not direct beneficiary/victim) experiences this as adherence to their own rules (permissive licenses conform to OSI definition; the extraction is technically legal). The engine computes directionality per seat from the authored beneficiary/victim and power/exit data: maintainer → high d (trapped + identity-locked + victim → near-full target); corporation → low d (institutional + arbitrage + beneficiary → near-full beneficiary); licensing body → symmetric (institutional, analytical, neither collecting nor paying from this constraint specifically).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is high (d → 0.85-0.95) for independent_open_source_maintainers: they are explicitly listed in victims[], their exit is identity_locked (switching to GPL means reframing their entire project and public positioning), their power is powerless, and they face institutional power (corporations) with arbitrage-grade exit. Directionality is low (d → 0.05-0.15) for enterprise_product_corporations: they are beneficiaries, they have institutional power and arbitrage-grade exit (can use any license framework in different product lines), and they extract measurably from the operation. Directionality is moderate (d → 0.40-0.55) for small_companies_competing_with_incumbents: they are victims (same code available to larger competitors), they have powerful-level individual power but constrained exit (cannot abandon the ecosystem entirely without exiting the market), and they face a structural asymmetry they did not create.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids false-classification as a pure rope by centering the maintainer's structural position: early in the interval (t=0-3), permissive licensing genuinely solved a coordination problem (compatibility friction, licensing complexity). By t=12-25, the founding problem is substantially resolved in the software industry, but the constraint persists and operates primarily to enable extraction. The founding_problem_status=contested reflects this: maintainers and academics say the problem is dead (we have good tools, GPL works fine, compatibility is understood); corporations and licensing advocates say it's live (GPL still creates friction, permissive adoption is higher). The theater_ratio trajectory (rising from 0.15 to 0.42) shows performative activity increasing: corporations increasingly emphasize their use of 'open-source values' and contribute token work to high-visibility projects, while their product strategy remains built on uncompensated-code extraction. The snare claim is justified: the constraint persists not because the coordination problem demands it, but because corporate beneficiaries suppress recognition of the extraction (through framing it as freedom/reuse) and because maintainers are identity-locked into permissive licensing ('I believe in open source, I can't switch to GPL without that feeling like a betrayal').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_reversibility,
    'Can maintainers break the identity-fusion with permissive-licensing ideology if they become aware of the extraction? Or is the identity-lock structural (career path, community reputation, self-concept) such that awareness alone does not enable exit?',
    'Longitudinal tracking of maintainers who switch to GPL after years of permissive licensing: do they report that awareness of extraction was sufficient, or did switching require other catalysts (explicit corporate behavior, external pressure, maintainer burnout)? Does exit remain costly even after decision?',
    'If identity-lock is reversible (awareness sufficient), suppression could be modeled as primarily cognitive/structural and accessible to intervention. If irreversible (requires external catalysts), suppression is deeper and the constraint operates at a higher effective extraction level than the scalar alone indicates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock to permissive-licensing ideology is reversible through awareness or requires external intervention.').

omega_variable(
    corporate_intent_and_structure,
    'Do enterprise corporations deliberately target permissive code for extraction, or is the extraction incidental to a generic ''reuse valuable code'' strategy? Is the suppression (framing extraction as ''freedom'') intentional obfuscation or genuine ideological alignment?',
    'Documentary evidence from corporate strategy documents, developer interviews, and acquisition patterns: do corporations deliberately seek permissive-licensed acquisitions? Do they avoid GPL code when permissive alternatives exist? What do internal discussions reveal about extraction intent?',
    'If intent is deliberate, suppression is coordinated fraud (high-confidence snare). If incidental, the constraint is a structural artifact of asymmetric power (still snare, but possibly remediable through licensing norm-shift). If corporations are genuinely ideologically aligned with permissive philosophy, the suppression is self-deception rather than intentional obfuscation (still snare, but the remedy is different).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_intent_and_structure, empirical, 'Whether corporate extraction is deliberate strategy, incidental artifact, or ideologically-justified.').

omega_variable(
    alternative_licensing_visibility,
    'Are copyleft and other reciprocal-licensing alternatives genuinely visible and available to new maintainers, or does institutional momentum and default-selection in tooling (GitHub, npm, etc.) obscure permissive alternatives?',
    'Analysis of license selection patterns on major code repositories: do new projects default to permissive or reciprocal? What do surveys of maintainers reveal about license-selection knowledge? Is the choice informed or habitual?',
    'If alternatives are obscured, accessibility_collapse is lower than authored (exits exist but are not recognized) and suppression includes structural information-asymmetry. If alternatives are visible but rejected, the maintainers are making an active ideological choice, which lowers both accessibility_collapse and suppression measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_licensing_visibility, empirical, 'Whether copyleft and reciprocal-licensing alternatives are visible to maintainers or obscured by institutional momentum.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'This constraint is ONE reading of the permissive_license_text kernel. Does the permissive text ITSELF enable or require the extraction this reading describes, or does extraction depend on corporate behavior and maintainer identity-lock that the text does not mandate?',
    'Genealogical and structural analysis: What does the MIT/Apache/BSD text explicitly permit and prohibit? What enforcement infrastructure exists? If corporations were held to reciprocal-share norms by maintainer pressure or legal interpretation, would the text remain permissive or would it become something else? The alternative (commons_coordination_reading) reads the same text as maximizing freedom without exploitation — by what structural or ethical criteria does that reading fail?',
    'If extraction is enabled-and-intended by the text, the text itself is the wrong starting commitment. If extraction depends on suppression and identity-lock, fixing the constraint requires intervention at those points (maintainer awareness, institutional norm-shift, legal/norm-based enforcement of reciprocity norms), not text change. The reading divergence points to whether the kernel (permissive text) is the problem or merely the vehicle for extraction that depends on other factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether extraction is enabled by the permissive-text kernel itself or depends on external suppression and identity-lock factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_corp_moat_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perm_corp_moat_tr_t3, permissive_license_text__corporate_moat_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement(perm_corp_moat_tr_t7, permissive_license_text__corporate_moat_reading, theater_ratio, 7, 0.25).
narrative_ontology:measurement(perm_corp_moat_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(perm_corp_moat_tr_t18, permissive_license_text__corporate_moat_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(perm_corp_moat_tr_t25, permissive_license_text__corporate_moat_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(perm_corp_moat_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perm_corp_moat_be_t3, permissive_license_text__corporate_moat_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(perm_corp_moat_be_t7, permissive_license_text__corporate_moat_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(perm_corp_moat_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(perm_corp_moat_be_t18, permissive_license_text__corporate_moat_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(perm_corp_moat_be_t25, permissive_license_text__corporate_moat_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_corp_moat_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(perm_corp_moat_su_t3, permissive_license_text__corporate_moat_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(perm_corp_moat_su_t7, permissive_license_text__corporate_moat_reading, suppression_requirement, 7, 0.59).
narrative_ontology:measurement(perm_corp_moat_su_t12, permissive_license_text__corporate_moat_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(perm_corp_moat_su_t18, permissive_license_text__corporate_moat_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(perm_corp_moat_su_t25, permissive_license_text__corporate_moat_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__corporate_moat_reading, 0.18).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The permissive_license_text kernel admits three structurally distinct constraint readings, each with different beneficiary/victim structures, epsilon values, and type classifications. commons_coordination_reading claims low extractiveness (rope) — permissive licensing solves a genuine coordination problem with mutual benefit. copyleft_counterfactual_reading claims high extractiveness (snare) — permissive licensing without reciprocity enables violation and extraction; GPL alternatives are necessary. corporate_moat_reading (this constraint) claims moderate-high extractiveness (snare) — uncompensated extraction by corporations, enabled by permissive-text permission and suppressed through 'freedom' framing. All three read the same kernel text; the readings diverge in what asymmetries and value flows the text enables. The network edge represents constraint-family structure: the commons reading influences both the copyleft and corporate readings (its framing of 'freedom' is the legitimacy both others must contend with); the corporate reading influences (but does not foreclose) the copyleft reading (evidence of extraction strengthens copyleft's argument for reciprocal alternatives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
