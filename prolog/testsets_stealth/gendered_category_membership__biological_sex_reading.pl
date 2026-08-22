% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological-Marker Grounding of Gendered Category Membership (biological sex reading)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   A legal and institutional arrangement grounds gendered category
 *   membership in immutable biological markers — chromosomes and reproductive
 *   anatomy observed at birth — and enforces the resulting boundary through
 *   statutory definitions, identity documents, single-sex provision rules,
 *   and sports eligibility regimes. Trans women are excluded from the 'woman'
 *   category and trans men from the 'man' category regardless of lived
 *   identity; intersex individuals are reclassified or tested at the
 *   boundary's edge; gender-nonconforming cis women absorb collateral
 *   enforcement. The reading presents the boundary as tracking a natural
 *   kind; the structural record shows an actively maintained legal construct
 *   with genuine coordination functions (administrable classification,
 *   provision allocation, athletic eligibility) and substantial asymmetric
 *   costs imposed on those it classifies against their identity. This story
 *   is a member of a three-reading constraint family over the
 *   gendered_category_membership kernel; each reading is a separate file with
 *   its own stable ε over the SAME standing arrangement — this reading
 *   authors ε 0.72 for the biological-marker regime it defends, and the
 *   sibling files author their own reading-indexed values with different
 *   victim sets. Interval mapping: T0-T30 approximates 1995-2025.
 *
 * KEY AGENTS:
 *   - sex_definition_legislators: agenda_setter (institutional/mobile) — writes and amends the criterion; exit is procedural but politically priced
 *   - sports_governing_bodies: agenda_setter (institutional/constrained) — administers the boundary in sport at global scope
 *   - cis_women: positioned beneficiary (organized/constrained) — hold exclusive access by birth biology; the reading narrates them as victims of category dilution
 *   - trans_women: primary target (powerless/identity_locked) — excluded from the category they live in; the only exit offered is the identity itself
 *   - trans_men: primary target (powerless/identity_locked) — classified into the category they left
 *   - intersex_individuals: target at the boundary's edge (powerless/trapped) — bodies the binary does not fit
 *   - gender_nonconforming_cis_women: enforcement casualties (moderate/constrained) — bear policing aimed at the boundary
 *   - gender_critical_feminist_organizations: ideological beneficiary (organized/constrained) — the definition is their organizing premise
 *   - religious_conservative_institutions: doctrinal beneficiary (organized/constrained) — the legal binary vindicates created-order teaching
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — review the arrangement without administering it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.72).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.68).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological-Marker Grounding of Gendered Category Membership (biological sex reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'e5926df1-9498-45bb-a321-eb3e1134beda').
narrative_ontology:cs_kernel_codification('e5926df1-9498-45bb-a321-eb3e1134beda', formalized).
narrative_ontology:cs_authority_grounding('e5926df1-9498-45bb-a321-eb3e1134beda', expertise).
narrative_ontology:cs_interpretation_layer_present('e5926df1-9498-45bb-a321-eb3e1134beda').
narrative_ontology:cs_reading_relation('e5926df1-9498-45bb-a321-eb3e1134beda', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('e5926df1-9498-45bb-a321-eb3e1134beda', gendered_category_membership__social_role_reading, forecloses).
narrative_ontology:cs_axiom('e5926df1-9498-45bb-a321-eb3e1134beda', foundational, category_membership_tracks_immutable_birth_biology).
narrative_ontology:cs_axiom_status(category_membership_tracks_immutable_birth_biology, holdable).
narrative_ontology:cs_axiom_grounding('e5926df1-9498-45bb-a321-eb3e1134beda', category_membership_tracks_immutable_birth_biology, empirically_contingent).
narrative_ontology:cs_axiom('e5926df1-9498-45bb-a321-eb3e1134beda', secondary, sex_segregated_provision_requires_biological_boundary).
narrative_ontology:cs_axiom_status(sex_segregated_provision_requires_biological_boundary, holdable).
narrative_ontology:cs_axiom_grounding('e5926df1-9498-45bb-a321-eb3e1134beda', sex_segregated_provision_requires_biological_boundary, instrumental).
narrative_ontology:cs_reference_frame('e5926df1-9498-45bb-a321-eb3e1134beda', immutable_binary_marker_grounding).
narrative_ontology:cs_drift_state('e5926df1-9498-45bb-a321-eb3e1134beda', contemporary_intersex_self_id_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5926df1-9498-45bb-a321-eb3e1134beda', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, gender_critical_feminist_organizations).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, religious_conservative_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_men).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, gender_nonconforming_cis_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, binary_sex_immutability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enact the statutory language that fixes legal category membership to chromosomes or birth-observed anatomy, and direct the document systems, facility rules, and eligibility regimes that apply it. They choose the criterion and can amend it; they gain constituency support from enforcement and bear electoral risk from reversal, so their freedom to leave the arrangement is the same pen that wrote it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sex_definition_legislators, agenda_setter,
    institutional, generational, mobile, national).

% Set and administer eligibility rules for women's categories using biological markers — birth-registered sex, chromosomal verification, testosterone thresholds. A bright-line biological criterion gives them rule clarity and litigation defensibility; they bear the costs of testing programs, athlete disputes, and scientific controversy over the edge cases their criterion keeps producing.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Hold membership by birth biology and receive the access the arrangement reserves to that membership: single-sex facilities, women's sports categories, and sex-based schemes built on the biological definition. They also carry costs where enforcement scrutinizes women's bodies — facility challenges and eligibility testing — and individually they cannot exit the category their biology assigns, though organized advocacy can campaign to redefine it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cis_women, payer).

% Live as women while being classified by the arrangement into their birth-registered category: documents that mismatch lived identity, refusal of access to women's provision, and a legal rule that treats the identity claim itself as void. The exit the arrangement offers is living as the birth-assigned category — relinquishing the lived identity — which is the harm itself rather than an alternative.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Live as men while remaining legally and provisionally classified female: placement in women's facilities against their interest, exclusion from men's categories, and documents that disclose their history whenever the birth-registered marker is checked. The exit on offer is the same — detransition into the birth-assigned category.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_men, payer,
    powerless, biographical, identity_locked, national).

% Born with variations in chromosomes, anatomy, or hormones that the binary the arrangement presumes does not fit. They face reclassification, infant normalization procedures historically justified by administrative convenience, and sex-testing regimes in sport that have repeatedly caught them. There is no exit: their bodies are the cases the boundary was not built for.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_individuals, payer,
    powerless, generational, trapped, national).

% Cis women whose appearance or physiology — masculine presentation, naturally high testosterone — triggers enforcement aimed at the boundary: facility challenges, eligibility testing, public suspicion. They nominally hold the protected position but absorb a share of the enforcement burden, and their recourse is documenting their biology to establish what their membership should make presumptive.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_nonconforming_cis_women, payer,
    moderate, biographical, constrained, national).

% Advocacy organizations organized around sex-based legal category; the biological definition is both their founding premise and their legislative objective. Codification campaigns bring them organizational salience, membership, funding, and policy influence; abandoning the definition would dissolve the identity and infrastructure built on it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_critical_feminist_organizations, beneficiary,
    organized, generational, constrained, national).

% Institutions whose teaching fixes sex as created, binary, and immutable. The legal binary vindicates that teaching and enlists state power in its maintenance; they gain doctrinal vindication and policy alignment, and conceding that legal category is a constructible artifact would undercut the created-order claim their authority rests on.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, religious_conservative_institutions, beneficiary,
    organized, civilizational, constrained, global).

% Treaty bodies, regional courts, and human rights mechanisms that review the arrangement against non-discrimination, privacy, and bodily-integrity norms. They take testimony from every other seat, publish findings that shift legitimacy between the competing definitions, and administer no category boundary themselves.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single administrable criterion for allocating sex-classified provisions — birth registration, identity documents, single-sex facilities, sports eligibility, medical protocols — that requires no individual testimony and can be verified against records held at birth.
% TRANSFER_FUNCTION: Moves recognition and access away from trans women and trans men (into their birth-registered category, with loss of document match, facility access, and identity legibility) and reserves the protected provisions for members classified by birth biology; enforcement costs — testing, scrutiny, litigation — are borne by trans and intersex individuals and by gender-nonconforming cis women caught in policing.
% ABSENT_VOICES: The people whose membership is being defined — trans women and trans men — are structurally absent from the legislative and rule-writing rooms where the criterion is fixed in codifying jurisdictions; intersex individuals are cited as boundary cases in policy debate but rarely seated in drafting. Their objections exist in testimony and litigation but enter only after the definition is written.
% DISAPPEARANCE_RATIONALE: Jurisdictions that replaced the biological criterion with self-declaration show what rearranges: document systems re-issue, single-sex provisions re-derive admission rules, sports categories rebuild eligibility criteria, prison placement rules change, and the enforcement machinery — testing, challenges, litigation — dismantles. Nothing in the underlying biology changes; the rearrangement is entirely in the legal and institutional architecture the criterion organizes.
% FOUNDING_PROBLEM: An administrable, dispute-free criterion for legal sex classification: registration and documentation systems needed a determinable, stable marker at a time when sex was presumed binary, unambiguous, and immutable, and sex-based protections, sports categories, and provision rules later inherited that criterion as their boundary.
% FOUNDING_PROBLEM_CORROBORATION: Vital-statistics historians and administrative-law scholars attest the original registration problem was real and is now largely solved by routine documentation; medical bodies attest a real but distinct clinical need for biological data; sports scientists attest fairness interests in eligibility criteria. No source outside the arrangement's beneficiaries attests that the founding problem requires excluding trans members from the legal category — that link is asserted only by the arrangement's defenders, and jurisdictions that decoupled clinical data from legal membership operate without it.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the arrangement's operative cost falls on a group that cannot pay it in any currency the arrangement recognizes: trans women and trans men are moved out of their lived category, their documents mismatch, their access is refused, and the identity claim itself is ruled void — the identity claim is the enforcement object, not a side effect. Suppression (0.68) reflects the enforcement machinery: statutory definitions, document systems, eligibility testing, facility policing, and litigation defending the boundary; suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation (note sports enforcement at global scope, where verification is hardest). Theater (0.42) is moderate and rising: a growing share of enforcement activity is symbolic — facility policing that catches almost no one, statutes aimed at scenarios that rarely arise — layered over real enforcement (sports testing, prison placement, document refusal). Accessibility collapse is moderate (0.50): self-ID regimes in multiple jurisdictions demonstrate the alternative functions, so alternatives remain visible and reachable. Resistance (0.72) is intense and organized: litigation, counter-legislation, institutional divergence, sustained protest. All three tracked series run on ONE shared six-point grid (alignment rule); the contest's episodic panic waves ride a monotonic rising baseline, so the 6-point grid captures the structural drift without mistaking wave amplitude for the signal. Suppression_requirement is tracked because enforcement capacity demonstrably built up over the interval (codification waves, testing regimes, document policies) — this is an enforcement-hardening story, not a static-enforcement story. Receipt surface: the arrangement's operation takes recognition and access from trans and intersex people as DENIAL rather than transfer — the denied good is not delivered to any seat. cis_women receive exclusive access (benefit of the arrangement, not receipt of the extraction); gender_critical_feminist_organizations and religious_conservative_institutions capture organizational and doctrinal gains from the contest AROUND the arrangement; no seat receives the extraction itself, so gain_flow is authored 'diffuse' as a checked claim over every named seat, not a default. fixing_cost is authored 'prohibitive' on political evidence: for the legislators who could redefine membership, the observed electoral cost of reversal in codifying jurisdictions has exceeded the politically discounted benefit every time it has been attempted — political prohibitive-ness, which does not make the arrangement inertial, because concentrated advocacy actively maintains it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seats (legislators, sports bodies) the arrangement is administrative clarity: one criterion, no individual testimony, verifiable records. From the cis_women beneficiary seat the arrangement is protection — and the biological reading's own narrative additionally positions cis women as victims of category dilution; the structural data authored here position them as the beneficiary class instead, with gender-nonconforming cis women absorbing a collateral enforcement burden the dilution narrative does not count. From the trans payer seats the same structure operates as identity suppression with no available exit that is not the suppression itself. From the intersex seat the binary is an administrative fiction their bodies falsify. The engine computes this divergence from power, exit, and directionality data; the reading's victim-of-dilution framing is recorded as the reading's framing, not as structural fact, and does not adjudicate the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   cis_women, gender_critical_feminist_organizations, and religious_conservative_institutions are declared beneficiaries and derive low directionality (subsidized or lightly burdened by the arrangement). trans_women, trans_men, and intersex_individuals are declared victims with identity_locked or trapped exit, placing them near the full-target end — identity_locked here is derived from the victim declarations plus the exit structure (the only exit on offer is relinquishing the identity), not from any fallback. gender_nonconforming_cis_women derive high directionality from their victim declaration despite nominal category membership — enforcement reaches them directly. sex_definition_legislators and sports_governing_bodies are neither declared beneficiaries nor victims, so they take the power-atom canonical fallback near symmetric — appropriate for seats that gain administrative authority from the boundary while bearing litigation and electoral costs; no per-agent directionality overrides are authored because the override surface keys on power_atom and would collide across these differently-positioned institutional seats. international_human_rights_bodies sit at the analytical seat with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an administrable, dispute-free sex-registration criterion — was real and is largely solved by routine documentation; the exclusionary use of the criterion is a contested extension. R5 status is therefore 'contested', not 'dead': the administrative need persists even where the exclusion does not, and the parties dispute whether the arrangement still solves anything the criterion alone could not. The mismatch consumer should find status=contested x verdict=world_rearranges — no dead-mandate zombie flag, with the residual dispute routed through the omegas. The classification prevents two opposite mislabels: calling this a snare would erase the genuine coordination function (administrable classification, provision allocation, athletic eligibility) that even hostile jurisdictions preserve in some form; calling it a rope would erase the asymmetric extraction that is the arrangement's operative fact for its targets. Active enforcement plus named beneficiaries and victims hold it at tangled_rope. The rising theater series is watched for piton drift, but the piton test fails structurally: concentrated advocacy (gender-critical organizations, religious institutions) profits enough to maintain the boundary actively — this is the opposite of inertial, theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the biological_sex_reading of the gendered_category_membership kernel; what would adoption of a sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative analysis of jurisdictions and institutions that have adopted each reading: the victim set, the enforcement object, and the cost structure invert under the gender_identity_reading (the enforcement machinery itself becomes the violation) and redistribute under the social_role_reading (membership follows sustained recognition, converting the boundary into a reputation mechanism).',
    'If the identity reading were adopted, trans women become members, the cis-women-as-victims-of-dilution framing dissolves as a category, and this story''s cost structure inverts; the disagreement is located in the grounding of category membership — immutable markers vs. self-declaration vs. sustained recognition — and no observation inside one reading''s framework resolves it for the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption inverts the victim set and the enforcement object.').

omega_variable(
    natural_boundary_vs_constructed_boundary,
    'Is the legal boundary a natural-kind fact that law merely tracks, or a constructed legal boundary that naturalizes itself by appeal to biology?',
    'Comparative jurisprudence: where jurisdictions redefined membership without any change in the underlying biology, the category rearranged — demonstrating the legal boundary is constructed even though the biology is not.',
    'If constructed, the arrangement''s naturality claims function as enforcement cover and false-summit pressure applies despite the tangled_rope claim; if the boundary merely tracks a natural kind, part of the measured cost is the price of tracking rather than of construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_boundary_vs_constructed_boundary, conceptual, 'Whether the constraint''s naturality framing survives the comparative record.').

omega_variable(
    collateral_enforcement_share,
    'What share of the enforcement burden falls on cis gender-nonconforming women and intersex individuals rather than on trans people?',
    'Enforcement records: facility-challenge reports, sports testing outcomes, and document disputes, classified by whether the person challenged was trans, intersex, or cis.',
    'A high collateral share weakens the arrangement''s protective coordination claim and pushes classification toward snare (enforcement whose coordination benefit is small relative to harm imposed); a low share supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collateral_enforcement_share, empirical, 'Distribution of enforcement harm across the victim seats.').

omega_variable(
    protection_exclusion_separability,
    'Are the protective functions (privacy in intimate provision, athletic fairness, medical accuracy) separable from the exclusion of trans members, or does the boundary''s protective function depend on the exclusion?',
    'Natural experiment from self-ID jurisdictions that maintained single-sex provision under alternative criteria (case-by-case admission rules): if protection outcomes hold while exclusion ends, the functions are separable.',
    'If separable, the exclusion is extraction riding on genuine coordination; if inseparable, part of the measured extraction is the price of the protection the reading exists to provide.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_exclusion_separability, empirical, 'Whether the coordination and exclusion components of the boundary are structurally separable.').

omega_variable(
    authority_grounding_framing,
    'Is this reading''s authority grounded in credentialed scientific expertise or in diffuse epistemic self-evidence (''everyone knows what a woman is'')?',
    'Examine which seat actually adjudicates edge cases: if sports eligibility panels and medical determination bodies decide, authority is expertise with a functioning interpretation layer; if the category is treated as requiring no adjudication and edge cases are explained away, authority is diffuse_epistemic with no interpreter.',
    'Expertise grounding routes drift through credentialed interpretation, absorbing intersex edge cases without surfacing kernel revision; diffuse_epistemic grounding has no interpreter, making the kernel brittle at every edge case and accelerating codification_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: expertise vs. diffuse_epistemic authority grounding for the same reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(gend_tr_t0, observed).
narrative_ontology:measurement(gend_tr_t6, gendered_category_membership__biological_sex_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(gend_tr_t6, observed).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__biological_sex_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(gend_tr_t12, observed).
narrative_ontology:measurement(gend_tr_t18, gendered_category_membership__biological_sex_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(gend_tr_t18, observed).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__biological_sex_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(gend_tr_t24, observed).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(gend_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(gend_be_t0, observed).
narrative_ontology:measurement(gend_be_t6, gendered_category_membership__biological_sex_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(gend_be_t6, observed).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__biological_sex_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(gend_be_t12, observed).
narrative_ontology:measurement(gend_be_t18, gendered_category_membership__biological_sex_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(gend_be_t18, observed).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__biological_sex_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(gend_be_t24, observed).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(gend_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(gend_su_t0, observed).
narrative_ontology:measurement(gend_su_t6, gendered_category_membership__biological_sex_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(gend_su_t6, observed).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__biological_sex_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(gend_su_t12, observed).
narrative_ontology:measurement(gend_su_t18, gendered_category_membership__biological_sex_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(gend_su_t18, observed).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__biological_sex_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(gend_su_t24, observed).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(gend_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the gendered_category_membership kernel (ε-invariance principle): the colloquial label 'what makes someone a woman/man' covers three structurally distinct constraints — biological-marker grounding (this file), self-declaration grounding, and social-performance grounding. Each is a separate story with its own stable ε, victim set, and enforcement object; they are linked here because the biological reading is cited as the settled baseline against which the siblings are argued, so its drift propagates legitimacy pressure downstream to both. The referent (the standing arrangement under contest) is shared; the ε values are reading-indexed and differ because each reading counts different harms as extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
