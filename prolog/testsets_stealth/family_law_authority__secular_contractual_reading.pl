% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Civil Marriage Registration Regime (Secular Contractual Reading of Family Law Authority)
 *   domain: comparative law/political theory/religious governance
 *
 * SUMMARY:
 *   In the secular contractual reading, marriage is a civil status
 *   constituted by state registration between two autonomous individuals,
 *   with gender-symmetric rights, no religious requirement, and interfaith
 *   marriage permitted. The standing arrangement this story assesses is the
 *   civil marriage regime as it actually operates: a state-administered
 *   validity gate conveying a large bundle of default rights (next-of-kin
 *   standing, succession, taxation, medical decision authority, immigration
 *   sponsorship) to registered couples, enforced by courts at dissolution.
 *   The regime solves a real coordination problem — third parties need one
 *   authoritative record of who holds family status — while reserving the
 *   bundle exclusively for the dyadic registered form and binding dissolution
 *   to state-set terms. Epsilon's referent is this standing arrangement
 *   assessed by the reading's own lights: the reading endorses
 *   registration-based validity and gender symmetry, so the extraction it
 *   registers is the residual state monopoly (fees, exclusivity of the
 *   bundle, non-waivable dissolution terms), not the validity criterion
 *   itself. The claim/metric gap is deliberate: claimed_type records the
 *   structure I believe true (a hybrid with genuine coordination and real
 *   asymmetry), while the metrics describe observed operation; the engine
 *   computes per-seat classifications from the structural data. KEY AGENTS
 *   (by structural relationship): - civil_marriage_registries: agenda-setting
 *   administrator (institutional/arbitrage) — operates the validity gate,
 *   collects fees - family_law_judiciary: agenda-setting enforcer
 *   (institutional/arbitrage) — adjudicates dissolution, sets doctrine -
 *   married_spouses: primary beneficiary with payer residue
 *   (moderate/constrained) — holds the bundle, bound by mandatory terms -
 *   status_reliant_institutions: secondary beneficiary (institutional/mobile)
 *   — keys eligibility off the registered status -
 *   unmarried_partners_denied_bundle: primary target (moderate/trapped) —
 *   bears the exclusivity of the bundle - polyamorous_families: primary
 *   target (moderate/trapped) — no registration path for their household form
 *   - divorcing_parties_under_mandatory_terms: secondary target
 *   (moderate/constrained) — private agreements yield to state floors -
 *   religious_officiants_without_civil_authority: excluded voice
 *   (organized/constrained) — solemnization civilly inert without
 *   registration - nonconjugal_caregivers: excluded voice (powerless/trapped)
 *   — exercise the bundle's functions without eligibility -
 *   comparative_family_law_scholars: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.41).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.26).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Civil Marriage Registration Regime (Secular Contractual Reading of Family Law Authority)").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative law/political theory/religious governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, 'efe628c6-b5dd-42dc-89b4-d2603899585c').
narrative_ontology:cs_kernel_codification('efe628c6-b5dd-42dc-89b4-d2603899585c', formalized).
narrative_ontology:cs_authority_grounding('efe628c6-b5dd-42dc-89b4-d2603899585c', lineage).
narrative_ontology:cs_interpretation_layer_present('efe628c6-b5dd-42dc-89b4-d2603899585c').
narrative_ontology:cs_reading_relation('efe628c6-b5dd-42dc-89b4-d2603899585c', family_law_authority__hindu_dharmashastra_reading, influences).
narrative_ontology:cs_reading_relation('efe628c6-b5dd-42dc-89b4-d2603899585c', family_law_authority__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('efe628c6-b5dd-42dc-89b4-d2603899585c', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('efe628c6-b5dd-42dc-89b4-d2603899585c', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('efe628c6-b5dd-42dc-89b4-d2603899585c', foundational, state_registration_sole_civil_validity).
narrative_ontology:cs_axiom_status(state_registration_sole_civil_validity, holdable).
narrative_ontology:cs_axiom_grounding('efe628c6-b5dd-42dc-89b4-d2603899585c', state_registration_sole_civil_validity, conventional).
narrative_ontology:cs_axiom('efe628c6-b5dd-42dc-89b4-d2603899585c', foundational, spousal_rights_gender_symmetric).
narrative_ontology:cs_axiom_status(spousal_rights_gender_symmetric, holdable).
narrative_ontology:cs_axiom_grounding('efe628c6-b5dd-42dc-89b4-d2603899585c', spousal_rights_gender_symmetric, deontological).
narrative_ontology:cs_axiom('efe628c6-b5dd-42dc-89b4-d2603899585c', secondary, religious_rite_civilly_inert_without_registration).
narrative_ontology:cs_axiom_status(religious_rite_civilly_inert_without_registration, holdable).
narrative_ontology:cs_axiom_grounding('efe628c6-b5dd-42dc-89b4-d2603899585c', religious_rite_civilly_inert_without_registration, conventional).
narrative_ontology:cs_axiom('efe628c6-b5dd-42dc-89b4-d2603899585c', secondary, marital_unity_husband_legal_representative).
narrative_ontology:cs_axiom_status(marital_unity_husband_legal_representative, overridden).
narrative_ontology:cs_axiom_grounding('efe628c6-b5dd-42dc-89b4-d2603899585c', marital_unity_husband_legal_representative, conventional).
narrative_ontology:cs_reference_frame('efe628c6-b5dd-42dc-89b4-d2603899585c', civil_registration_sole_validity_symmetric_terms).
narrative_ontology:cs_drift_state('efe628c6-b5dd-42dc-89b4-d2603899585c', contemporary_cohabitation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('efe628c6-b5dd-42dc-89b4-d2603899585c', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, civil_marriage_registries).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, family_law_judiciary).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, married_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, status_reliant_institutions).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, unmarried_partners_denied_bundle).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, polyamorous_families).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, divorcing_parties_under_mandatory_terms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, married_spouses).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, civil_registration_validity_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, spousal_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the registration system that constitutes marital validity: issues licenses, records marriages, and maintains the public record third parties consult. Collects registration and certified-copy fees. Writes and revises the procedural rules and can restructure the validity criteria by legislation, so its relationship to the arrangement is fully revisable from the inside.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_marriage_registries, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, civil_marriage_registries, beneficiary).

% Adjudicates dissolution, property division, custody, and spousal support; its interpretation of the marriage code determines what the status actually confers. The docket and the doctrine are its own professional domain, and it can reshape the arrangement's operative terms through precedent without external permission.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, family_law_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive the status bundle: next-of-kin standing, inheritance defaults, tax treatment, medical decision authority, immigration sponsorship, and evidentiary privileges. Pay registration fees and, at dissolution, filing costs; their private agreements about custody, child support, and in many jurisdictions spousal support are bounded by state-set floors and ceilings. Leaving the status runs only through the state's dissolution process.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, married_spouses, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, married_spouses, payer).

% Insurers, employers, hospitals, pension administrators, and immigration authorities key eligibility, benefits, and decision hierarchies off the registered status rather than investigating each household. They could adapt to alternative status records at re-engineering cost, and some already maintain parallel tracks where partnership statutes exist.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, status_reliant_institutions, beneficiary,
    institutional, generational, mobile, national).

% Long-term cohabiting couples who want next-of-kin standing, inheritance defaults, or spousal benefits but cannot or will not enter the registered status. Where no partnership track exists, no alternative channel conveys the bundle; they bear the gap in hospital access, succession, and benefits, and can close it only by marrying.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, unmarried_partners_denied_bundle, payer,
    moderate, biographical, trapped, national).

% Multi-partner households in which more than two adults function as parents, partners, or caregivers. Registration accommodates exactly two spouses, so the household cannot obtain the bundle for its actual structure in any jurisdiction; members hold powers of attorney and wills as partial, costlier substitutes.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, polyamorous_families, payer,
    moderate, biographical, trapped, national).

% Parties dissolving a marriage whose private agreements yield to state-set terms: custody arrangements and child support cannot be contracted away, and several jurisdictions cap or void spousal-support waivers. They can settle within those bounds but cannot opt out of the process or the floors.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, divorcing_parties_under_mandatory_terms, payer,
    moderate, biographical, constrained, national).

% Clergy and religious bodies whose solemnization carries no civil effect unless they register with the state or the couple separately completes civil registration. They can accept state authorization on state terms or remain ceremonially active but civilly inert; their account of what constitutes a valid marriage is not part of the civil validity conversation.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_officiants_without_civil_authority, excluded,
    organized, generational, constrained, national).

% Adult children, siblings, long-term companions, and other non-spousal caregivers who perform the functions the bundle supports — medical decisions, hospital access, inheritance — without eligibility for it. They have no organized advocacy presence in family-code revision and no registration path to the standing they exercise in fact.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, nonconjugal_caregivers, excluded,
    powerless, biographical, trapped, national).

% Study how different validity criteria, gender structures, and registration systems shape household outcomes across jurisdictions; they document the arrangement's operation but collect nothing from it and bear none of its costs.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, comparative_family_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, civil_marriage_registries).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constitutes a single authoritative family status: one registration record that hospitals, succession law, tax authorities, pension systems, and immigration authorities can consult to determine who holds next-of-kin standing, default inheritance rights, and spousal benefits, so these need not be renegotiated or verified per household.
% TRANSFER_FUNCTION: Moves registration and dissolution fees from couples to the state; moves authority over dissolution terms (custody, child support, spousal support bounds) from private agreement to courts; conveys the status bundle to registered couples at the price of state-set terms; and, historically within the interval, moved property and custody defaults along gender lines — a transfer now formally symmetric.
% ABSENT_VOICES: Polyamorous families, non-conjugal caregivers, and unmarried partners have no seat in family-code revision — the code is written for the dyadic registered form and they bear its exclusivity without being consulted. Religious bodies without civil registration authority are heard only when legislatures choose to consult them. Marriage abstainers who bear non-marriage penalties in tax and benefits are unorganized and largely absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the registration regime vanished overnight, every existing marriage would lose its legal instrument: succession would default to intestacy rules keyed to nothing, hospital decision authority would collapse into per-institution discretion, immigration sponsorship and spousal benefits would lose their eligibility key, and courts would lose the dissolution jurisdiction through which most families exit the status. Third parties would need per-couple verification or private reconstruction of the entire bundle — a large-scale rearrangement, not a continuation.
% FOUNDING_PROBLEM: Pre-modern states needed a legible public record of household formation to administer property succession, the legitimacy of children, and taxation; the liberal recasting of the nineteenth and twentieth centuries rebuilt that record-keeping as the registration of a contract between autonomous individuals with symmetric default rights.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: hospital administrators and pension funds attest they determine next-of-kin and beneficiary eligibility from registration records; immigration authorities attest spousal sponsorship keys off registration; historical scholarship on succession and legitimacy records is independent of the state's fiscal interest. No corroborating source outside the beneficiary set attests that the gender-asymmetric founding terms remain operative.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.41, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.41) is moderate: registration and dissolution fees are small, but the validity monopoly reserves a large bundle for one relational form and binds dissolution to non-waivable terms, so the constraint's costs fall chiefly on those outside or leaving the status. Suppression (0.26) is low-moderate: the regime no longer needs heavy enforcement — the fault machinery, miscegenation enforcement, and prosecution-driven policing of the 1950s have been dismantled — but the bundle remains unreachable by any alternative channel where no partnership statute exists. Theater (0.15) is low: registration is functional record-keeping; the high-theater era was the fault-divorce regime, whose manufactured testimony and collusive evidence collapsed after no-fault reform — visible as the steep early decline in the theater series. Accessibility_collapse (0.48): once the bundle's value is understood, alternatives (cohabitation contracts, powers of attorney, partnership tracks where enacted) recover only part of it, so alternatives partly collapse but do not vanish. Resistance (0.35): marriage skeptics, religious objectors to civil terms, and exclusion advocates press the regime's margins without threatening its core; most parties opt in. The three series share one time grid (1955-2025, decade steps); the suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change — machinery built down across the interval — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (registries, judiciary) should compute the arrangement as coordination they administer and benefit from; married spouses compute it as a net benefit bounded by mandatory terms; the exclusion seats (unmarried partners, polyamorous families, non-conjugal caregivers) compute the same structure as a gate that withholds standing for functions they already perform. The engine derives these divergences from the declared beneficiary/victim structure and exit options; the authored claim does not adjudicate them. Inter-institutionally, the registries and the judiciary hold the same nominal power but different relationships: the registries collect the fees, the judiciary collects jurisdiction and doctrine-shaping discretion — the mandatory-terms omega marks that the judiciary's position may sit higher than its agenda-setter derivation suggests.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil registries and the judiciary sit near the beneficiary end: they set and administer the validity gate and collect fees and jurisdiction. Status-reliant institutions collect the coordination benefit without running the arrangement, with mobile exit keeping them near the beneficiary pole. Married spouses are declared beneficiaries but their exit runs through the state's own dissolution process and mandatory terms bind their private agreements, so their true position sits nearer symmetric than a pure beneficiary — I considered an override, but the override surface is keyed by power atom, and four agents share the moderate atom with different positions, so a power-atom-wide override would misapply; the structural declarations (beneficiary role, payer residue, constrained exit) carry the differentiation instead. Unmarried partners and polyamorous families sit near the full-target end: they bear the monopoly's costs with no exit channel. Divorcing parties bear the mandatory terms with constrained exit. Religious officiants bear a real cost — their solemnization is civilly inert — without being declared victims, since under this reading the civil validity function was never theirs to lose; their position sits above symmetric through the excluded-role derivation rather than a victim declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — authoritative family-status records for succession, legitimacy, and third-party reliance — is live, so the regime is not mandatrophy-resolved and the classification should not drift toward the degraded type on inertia grounds. The within-interval atrophy is real but component-level: the fault-adjudication mandate died with no-fault reform, and its theatrical residue is what the theater series records. Keeping the claim as a hybrid prevents two mislabels: a pure-coordination reading would erase the exclusion victims and the mandatory-terms payers; a pure-extraction reading would erase the bundle's genuine reliance value for hospitals, pensions, and immigration. The mismatch consumer reading founding_problem_status (live) against disappearance_verdict (world_rearranges) should find no zombie flag: the arrangement persists because its problem persists, not because its function has died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundle_exclusivity_necessity,
    'Is reserving the legal bundle for the dyadic registered form structurally necessary for third-party reliance, or could the bundle be conveyed across additional relational forms without losing the coordination value?',
    'Natural experiments from jurisdictions that enacted partnership or solidarity registers alongside marriage: if hospital, succession, and benefits reliance functions held after extension, exclusivity is not necessary.',
    'If exclusivity is not necessary, the exclusion of unmarried partners, polyamorous families, and non-conjugal caregivers is rent riding on the coordination function rather than its price; if necessary, part of the measured extraction is the coordination cost itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_exclusivity_necessity, empirical, 'Whether the validity monopoly is load-bearing for third-party reliance or a rent.').

omega_variable(
    autonomy_premise_fiction,
    'How operative is the reading''s own premise that marriage contracts autonomous individuals, given formation-time power asymmetries and dissolution-time non-waivable terms?',
    'Empirical study of negotiation conditions at formation (access to independent legal advice, information asymmetry) and of prenuptial agreement enforceability across jurisdictions.',
    'If the premise is substantially fictional, effective extraction from the weaker spouse is higher than the symmetric-form metrics suggest, and the reading''s legitimating account undercuts its own arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_premise_fiction, empirical, 'Whether the autonomous-individual premise is operative or fictional.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (secular_contractual_reading) of the family_law_authority kernel; the sibling readings (hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading) would relocate validity in religious rites and restructure spousal terms — which structural element do the readings actually contest?',
    'The disagreement is located in the source of marital validity (state registration versus religious rite) and the symmetry of spousal terms; comparative institutional analysis of jurisdictions where readings compete shows which element drives divergent outcomes.',
    'A sibling reading''s instantiation would change the victim set (interfaith couples barred, gender-asymmetric duties) and re-key the measured extraction onto doctrinal enforcement rather than the state monopoly; this file''s epsilon would not transfer to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which element of the kernel the readings contest.').

omega_variable(
    cohabitation_coverage_drift,
    'Is the registration regime''s shrinking coverage — rising cohabitation and courts extending quasi-marital consequences outside registration — drift the reading must absorb, or a stable equilibrium of differentiated statuses?',
    'Longitudinal comparison of jurisdictions extending cohabitant rights versus withholding them, and of marriage-rate response to each policy.',
    'Continued drift would push practice further from the registration-only reference frame and enlarge the class bearing the monopoly''s costs; stabilization would leave the current structure as the long-run form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_coverage_drift, empirical, 'Whether practice drift away from registration-only validity continues.').

omega_variable(
    mandatory_terms_justification,
    'Are the non-waivable dissolution terms (custody and child-support floors, spousal-support waiver limits) protective of third parties — children, creditors — or jurisdiction rent for the courts that administer them?',
    'Comparative outcomes in jurisdictions with more waivable terms, controlling for child outcomes and litigation volume.',
    'If rent, the judiciary''s position sits higher toward the target end than its agenda-setter derivation suggests and part of the measured extraction re-attributes to the dissolution process; if protective, those terms belong to the coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatory_terms_justification, conceptual, 'Whether mandatory dissolution terms are protective or rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__secular_contractual_reading, theater_ratio, 1955, 0.34).
narrative_ontology:measurement(fami_tr_t1965, family_law_authority__secular_contractual_reading, theater_ratio, 1965, 0.32).
narrative_ontology:measurement(fami_tr_t1975, family_law_authority__secular_contractual_reading, theater_ratio, 1975, 0.26).
narrative_ontology:measurement(fami_tr_t1985, family_law_authority__secular_contractual_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(fami_tr_t1995, family_law_authority__secular_contractual_reading, theater_ratio, 1995, 0.19).
narrative_ontology:measurement(fami_tr_t2005, family_law_authority__secular_contractual_reading, theater_ratio, 2005, 0.17).
narrative_ontology:measurement(fami_tr_t2015, family_law_authority__secular_contractual_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(fami_tr_t2025, family_law_authority__secular_contractual_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(fami_be_t1955, family_law_authority__secular_contractual_reading, base_extractiveness, 1955, 0.62).
narrative_ontology:measurement(fami_be_t1965, family_law_authority__secular_contractual_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(fami_be_t1975, family_law_authority__secular_contractual_reading, base_extractiveness, 1975, 0.51).
narrative_ontology:measurement(fami_be_t1985, family_law_authority__secular_contractual_reading, base_extractiveness, 1985, 0.47).
narrative_ontology:measurement(fami_be_t1995, family_law_authority__secular_contractual_reading, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement(fami_be_t2005, family_law_authority__secular_contractual_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(fami_be_t2015, family_law_authority__secular_contractual_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(fami_be_t2025, family_law_authority__secular_contractual_reading, base_extractiveness, 2025, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1955, family_law_authority__secular_contractual_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(fami_su_t1965, family_law_authority__secular_contractual_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement(fami_su_t1975, family_law_authority__secular_contractual_reading, suppression_requirement, 1975, 0.44).
narrative_ontology:measurement(fami_su_t1985, family_law_authority__secular_contractual_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(fami_su_t1995, family_law_authority__secular_contractual_reading, suppression_requirement, 1995, 0.34).
narrative_ontology:measurement(fami_su_t2005, family_law_authority__secular_contractual_reading, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement(fami_su_t2015, family_law_authority__secular_contractual_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(fami_su_t2025, family_law_authority__secular_contractual_reading, suppression_requirement, 2025, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% Family law authority is a contested kernel, not one constraint: each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and enforcement form. This reading (civil registration as sole validity, gender-symmetric terms) extracts through the state monopoly on validity and dissolution; the religious readings extract through doctrinal terms and communal enforcement. The structural relation runs through institutionalization: where this reading's civil marriage acts spread, they change the operating environment of the religious readings — their rites become civilly inert without registration — without logically eliminating them, hence the influences edges to the dharmic and shariat readings and coexistence edges to the canonical and parsi readings, whose dual-track equilibria are stable. The sibling files must carry the reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
