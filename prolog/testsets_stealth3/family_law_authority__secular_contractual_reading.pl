% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Civil Marriage Validity Regime — State-Contractual Reading
 *   domain: legal/political/comparative-law
 *
 * SUMMARY:
 *   The standing arrangement this story authors is civil marriage as the
 *   secular-contractual reading constitutes it: two autonomous adults
 *   contract before a state-authorized registrar, validity flows solely from
 *   entry in the civil record, rights and obligations inside the union are
 *   statutorily gender-symmetric, and no religious rite is required — nor
 *   sufficient — for civil validity. Around that validity criterion sits a
 *   large bundled rule-set: succession defaults, next-of-kin medical
 *   standing, spousal immigration routes, tax-filing options, survivor
 *   benefits, parental-responsibility presumptions. Admission to the bundle
 *   runs through one gate; departure runs through the same machinery at a
 *   marked-up price. The claim/metric split is deliberate: claimed_type
 *   records the structural judgment (a genuine coordination core carrying
 *   asymmetric, actively enforced extraction), while the metrics record
 *   descriptive operating levels; the engine computes per-seat types from the
 *   structural data, and divergence between claim and computed output is the
 *   measurement the corpus exists to take. Interval anchoring assumption: T0
 *   approximates 1965 (the opening of the equal-rights and decriminalization
 *   reform wave), T60 approximates 2025; time points index elapsed years. KEY
 *   AGENTS (by structural relationship): - registered_married_spouses
 *   (moderate/constrained): principal holders of the incident bundle -
 *   dependent_minor_children (powerless/trapped): ride parental registration
 *   for custody, support, and succession defaults -
 *   state_fiscal_administration (institutional/arbitrage): receives fee
 *   revenue and administrative economies from the household-counting unit -
 *   state_legislature (institutional/arbitrage): sets and revises validity
 *   criteria and dissolution procedure - civil_registration_authorities
 *   (institutional/arbitrage): administer the validity gate, with a
 *   fee-collection sideline - divorcing_spouses (moderate/constrained): bear
 *   the exit price — fees, waiting periods, litigation -
 *   unregistered_cohabiting_partners (moderate/mobile): live the joint life
 *   without the bundle, patch gaps with private instruments -
 *   polyamorous_households (powerless/trapped): no registration path exists
 *   at any price - means_tested_low_income_couples (powerless/trapped): face
 *   benefit cliffs that price registration out of reach -
 *   unlicensed_religious_communities (organized/identity_locked): ceremonies
 *   they regard as constitutive confer no civil status here -
 *   human_rights_treaty_bodies (institutional/analytical): audit statutory
 *   symmetry against administered outcomes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.42).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.48).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Civil Marriage Validity Regime — State-Contractual Reading").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "legal/political/comparative-law").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, 'a941da8d-5588-4072-b3dd-b5651e905ba7').
narrative_ontology:cs_kernel_codification('a941da8d-5588-4072-b3dd-b5651e905ba7', formalized).
narrative_ontology:cs_authority_grounding('a941da8d-5588-4072-b3dd-b5651e905ba7', lineage).
narrative_ontology:cs_interpretation_layer_present('a941da8d-5588-4072-b3dd-b5651e905ba7').
narrative_ontology:cs_reading_relation('a941da8d-5588-4072-b3dd-b5651e905ba7', family_law_authority__christian_canonical_reading, forecloses).
narrative_ontology:cs_reading_relation('a941da8d-5588-4072-b3dd-b5651e905ba7', family_law_authority__hindu_dharmashastra_reading, forecloses).
narrative_ontology:cs_reading_relation('a941da8d-5588-4072-b3dd-b5651e905ba7', family_law_authority__muslim_shariat_reading, forecloses).
narrative_ontology:cs_reading_relation('a941da8d-5588-4072-b3dd-b5651e905ba7', family_law_authority__parsi_zoroastrian_reading, forecloses).
narrative_ontology:cs_axiom('a941da8d-5588-4072-b3dd-b5651e905ba7', foundational, mutual_consent_of_autonomous_adults_is_constitutive).
narrative_ontology:cs_axiom_status(mutual_consent_of_autonomous_adults_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('a941da8d-5588-4072-b3dd-b5651e905ba7', mutual_consent_of_autonomous_adults_is_constitutive, deontological).
narrative_ontology:cs_axiom('a941da8d-5588-4072-b3dd-b5651e905ba7', foundational, state_registration_is_sole_validity_criterion).
narrative_ontology:cs_axiom_status(state_registration_is_sole_validity_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a941da8d-5588-4072-b3dd-b5651e905ba7', state_registration_is_sole_validity_criterion, conventional).
narrative_ontology:cs_reference_frame('a941da8d-5588-4072-b3dd-b5651e905ba7', state_registered_symmetric_consent_union).
narrative_ontology:cs_drift_state('a941da8d-5588-4072-b3dd-b5651e905ba7', contemporary_partnership_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a941da8d-5588-4072-b3dd-b5651e905ba7', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, registered_married_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, dependent_minor_children).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_fiscal_administration).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, divorcing_spouses).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, unregistered_cohabiting_partners).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, polyamorous_households).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, means_tested_low_income_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, civil_registration_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Two adults who registered and now hold the bundle: succession defaults, next-of-kin standing, filing options, spousal visa routes, survivor benefits. Their exit from the status runs back through the same courts and registries, at a price set by the same statute that admitted them.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, registered_married_spouses, beneficiary,
    moderate, biographical, constrained, national).

% Receive custody, support, and succession defaults keyed to their parents' registration. Cannot opt in or out; their protection depends entirely on whether the adults above them passed through the validity gate.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, dependent_minor_children, beneficiary,
    powerless, biographical, trapped, national).

% Tax and social-security administrations treat the registered couple-plus-dependents as a counting unit. Joint assessment options, survivor benefits, and household means-tests all key off the civil record; the unit yields administrative economies and a stable fee revenue stream.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_fiscal_administration, beneficiary,
    institutional, generational, arbitrage, national).

% Writes and amends the validity criteria, capacity rules, and dissolution procedure. Consults bar associations, licensed religious bodies, and family-policy advocacy groups; revisits the statute when electoral coalitions shift, not when unregistered or multi-partner households petition.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate the registers that confer validity: verify identity, capacity, and prior-bond status; collect fees; transmit certified extracts to courts, tax offices, and border agencies. Budgets and headcount scale with registration volume and with fraud-screening workload.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_registration_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, civil_registration_authorities, beneficiary).

% Dissolving the union runs through filing fees, mandated separation intervals where they apply, asset-division proceedings, and support recalculation. Statutory rights are written symmetrically, but administered outcomes — post-divorce income trajectories especially — remain uneven across spouses.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, divorcing_spouses, payer,
    moderate, biographical, constrained, national).

% Share homes, finances, and often children without registering. Hold no automatic succession, next-of-kin, or spousal-immigration standing; purchase partial substitutes — cohabitation contracts, powers of attorney, beneficiary designations — which work until a counterparty (a hospital desk, a border agency, an intestate estate) refuses them.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, unregistered_cohabiting_partners, payer,
    moderate, biographical, mobile, national).

% Multi-partner households raising children and sharing property. The registration template admits exactly two principals, so there is no application to file, no fee to pay, no path at any price. The household reconstructs fragments of the bundle through deeds, wills, guardianship petitions, and visitation authorizations that institutions may decline to honor.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, polyamorous_households, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, polyamorous_households, excluded).

% Live month-to-month on means-tested supports. Registering merges household income counts and can strip housing, childcare, and health benefits worth more than any advantage the bundle offers; the result is a standing penalty attached to the validity gate for exactly the households with the thinnest margins.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, means_tested_low_income_couples, payer,
    powerless, immediate, trapped, national).

% Communities whose own rites they regard as fully constituting marriage, but who either lack or decline state authorization to solemnize. Under the standing arrangement their ceremonies create no civil status, and they hold no seat in the consultations where validity criteria are written.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, unlicensed_religious_communities, excluded,
    organized, generational, identity_locked, national).

% Treaty-monitoring committees review national marriage and family law against equality and private-life provisions; they gather statistics, publish concluding observations, and document gaps between written symmetry and administered results. They impose no domestic rule directly.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, state_fiscal_administration).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one authoritative public record of conjugal bonds plus a standardized default rule-set — succession, next-of-kin medical standing, parental responsibility, spousal immigration, tax filing — so that registries, hospitals, courts, banks, and border agencies coordinate against a single status instead of negotiating bespoke terms with every household.
% TRANSFER_FUNCTION: Concentrates the legal incident bundle in registered dyads; moves registration and dissolution fees from individuals to public treasuries; moves documentary burdens (proof of capacity, prior-bond status) onto applicants; and centralizes definitional authority over the institution's terms in legislatures and family-law courts rather than in the contracting parties or their communities.
% ABSENT_VOICES: Multi-partner households (no seat in the two-principal template's drafting or amendment), unlicensed religious communities (whose constitutive claims carry no weight in validity consultations), and advocates of menu-style partnership instruments (who would unbundle the incidents) all stand outside the consultation circuit, which convenes bar associations, licensed clergy, and family-policy advocacy organized around the registered dyad.
% DISAPPEARANCE_RATIONALE: Succession would revert to general intestacy rules with no spousal preference, next-of-kin medical standing would dissolve into case-by-case dispute, spousal immigration routes would close, survivor pensions and filing options would lapse, and every third party that currently reads the civil record would need a new protocol overnight — thousands of statutory cross-references would require rewriting.
% FOUNDING_PROBLEM: As successor states took marriage registration over from ecclesiastical and customary record-keepers in the eighteenth and nineteenth centuries, the problem was proof: who is bound to whom, to whom do children belong for succession and support, and how are clandestine, fraudulent, or bigamous unions excluded — answered by a single state-maintained register with capacity screening.
% FOUNDING_PROBLEM_CORROBORATION: External attestation exists: historiography of civil-registration reform (the Hardwicke Act 1753 literature and Code Civil scholarship), the parliamentary record of the 1836 English Marriage Act debates, and demographic archives of clandestine-marriage litigation attest the recording problem and its state-register solution from outside any current beneficiary seat. Contemporary liveness is attested by family-court caseloads and registry fraud-screening volumes. Contestation of the full bundle's necessity comes from family-pluralism scholarship and partnership-diversity advocacy, neither of which disputes the recording core. No source outside the beneficiary set attests that the complete modern bundle is required by the original problem — that claim is asserted only from within.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.42: the coordination spine is real and heavily used, but the single-gate bundle design prices entry and exit above a bare-contract baseline, excludes entire household forms, and hands the fiscal administration a convenient counting unit — extraction that survives even this reading's own autonomy-justifying lights, since a consent-centered reading implies freedom of terms that the standardized, non-negotiable bundle denies. Suppression 0.48: opting out entirely carries no sanction, but within the recognized-partnership domain the state's monopoly on validity collapses most alternatives; partial private substitutes exist and fail unpredictably at hospital desks and borders. Theater 0.18: the machinery performs its function; ceremony and commemorative politics add performance around a working core. Accessibility_collapse 0.45: understanding the gate does not erase alternatives — private contracts and parallel partnership lanes exist in some jurisdictions — but each substitute covers only a fragment of the bundle. Resistance 0.40: cohabitation growth, marriage delay, principled-refusal currents, and partnership-diversity advocacy press against the constraint without displacing it. Trajectories: base_extractiveness declines across the interval (final coverture-era residues removed, no-fault dissolution cutting forced-maintenance costs, morality-policing enforcement retired), flattening late as remaining extraction concentrates in bundle-design features rather than role asymmetry; suppression_requirement falls faster early (criminal enforcement retired) then stabilizes with a late uptick as documentary-fraud screening intensified; theater_ratio creeps upward as commemorative and defensive politics accumulate around a stable core. Enforcement history is traced because the story's dynamic IS enforcement-capacity change (criminal-to-administrative migration); all three series share one seven-point grid, and end-state values match the base_properties scalars by construction of the interval ending at the present.
 *
 * PERSPECTIVAL GAP:
 *   From the registered-spouse seat the arrangement presents as enabling infrastructure — a cheap key to a thousand legal doors. From the divorcing seat the same machinery presents as a tollgate with the toll collected on the way out; from the means-tested seat the gate is priced above reach precisely where its protections matter most; from the registry seat it is a workflow and a budget line. Same statute, four different lived arrangements. The engine computes these per-seat classifications from the structural declarations; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: registered spouses, dependent children, and the fiscal administration sit near the subsidized end, with the children's powerlessness and trapped exit reinforcing subsidy-side treatment of that seat. Victim declarations drive the target end: polyamorous households and means-tested couples combine victim position with trapped exit, placing them near full-target; divorcing spouses are victims whose exit runs through the constraint's own machinery (constrained, high d). Unregistered cohabitants are the modulation case: declared victims, but their mobile exit and access to partial private substitutes pull their effective d below the trapped-victim band — exit-option differentiation does the work a per-agent override would otherwise perform, so no directionality_overrides entries are authored. Scope is national for nearly all seats, so scope amplification of effective extraction is modest; the human-rights seat observes at global scope from an analytical position outside the d computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a single authoritative public record of conjugal bonds for succession, support, and proof against clandestine unions — retains a live core: registries still adjudicate capacity, prior-bond status, and document fraud daily, and courts still consume the record. Whether the full institutional bundle remains necessary to that recording function is disputed by the parties, hence founding_problem_status 'contested' rather than 'dead'. Combined with a world_rearranges disappearance verdict, the mismatch consumer reads contested x rearranges and raises no dead-mandate zombie flag — correctly, since the recording core is demonstrably exercised. The tangled_rope claim is what prevents mislabeling in both directions: the genuine coordination function (the register would be rebuilt within a year if abolished) blocks a pure-extraction reading, while the measurable asymmetric extraction (exit pricing, exclusion of non-dyadic forms, benefit-cliff incidence at the gate) blocks a pure-coordination reading. No sunset clause is authored because the arrangement carries no declared transition endpoint; its justification is steady-state, not transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint instantiates the secular_contractual_reading of the family_law_authority kernel; what would displacement by a sibling reading (hindu_dharmashastra, muslim_shariat, christian_canonical, parsi_zoroastrian) change structurally?',
    'Compile and compare the sibling reading stories: shifts in the validity criterion (rite or text versus state registration), in the victim sets (for example, wives under role-defined asymmetric divorce authority, interfaith couples under endogamy requirements), and in the enforcement authority locate what each sibling would alter.',
    'If a sibling reading displaced this one, the validity gate moves from the civil register to a religious authority, the victim set expands along religious and gender lines, and the epsilon referent becomes a different standing arrangement entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: this story is one reading of the contested family-law-authority kernel.').

omega_variable(
    disagreement_locus_across_readings,
    'Where exactly do the sibling readings disagree — at the source of validity (state versus religious authority), at the allocation of rights inside the union (symmetric versus role-defined), or at the boundary of permissible unions (interfaith permission versus endogamy)?',
    'Structural comparison of foundational axioms across the sibling set: each reading''s distinguishing atom identifies which element it treats as constitutive, and the overlap pattern isolates the primary contested locus.',
    'If the primary locus is validity-source, this reading''s sole-criterion axiom is the contested edge and its foreclosure relations carry the load; if rights-allocation dominates, the flashpoint is statutory amendment inside this reading, and its extraction profile moves with each family-law revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_locus_across_readings, conceptual, 'Locus of the kernel disagreement across sibling readings.').

omega_variable(
    bundle_indivisibility_necessity,
    'Is the standardized all-or-nothing incident bundle structurally necessary for third-party legibility (registries, hospitals, courts, banks coordinating on one status), or could menu-style partnership instruments deliver the same coordination at lower extraction?',
    'Compare administrative error rates and transaction costs in jurisdictions operating parallel lighter instruments alongside full marriage against jurisdictions offering only the full bundle.',
    'If the functions are separable, the gap between bundle cost and modular cost is excess extraction attributable to this constraint''s design choice; if inseparable, part of the measured extraction is irreducible coordination cost and the tangled_rope balance shifts toward the rope pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_indivisibility_necessity, empirical, 'Whether the single-bundle design is coordination-necessary or rent-bearing.').

omega_variable(
    dissolution_cost_attribution,
    'Given gender-symmetric statutes, do dissolution costs and post-dissolution outcomes fall evenly across spouses, or do observed gaps indicate residual asymmetry inside the constraint''s own procedure?',
    'Longitudinal income and caretaking-loss studies of divorced cohorts, controlling for pre-dissolution earnings, labor-force attachment, and child-rearing shares.',
    'Persistent controlled gaps would raise the payer-seat extraction estimate for this constraint and point to dissolution-procedure remedies; parity would corroborate the symmetry principle the arrangement vindicates and lower the divorcing-seat d.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissolution_cost_attribution, empirical, 'Whether symmetric statutes produce symmetric dissolution outcomes.').

omega_variable(
    benefit_cliff_coupling_attribution,
    'Are the registration penalties borne by means-tested couples attributable to this constraint''s indivisible-bundle design, or to the parallel means-testing regime''s household-counting rules?',
    'Natural experiment: jurisdictions that severed fiscal household-unit definitions from marital status (individualized taxation and benefit assessment) — do marriage rates and measured registration penalties diverge there?',
    'If attributable to the bundle design, means-tested couples sit firmly in this constraint''s victim set and its extraction estimate stands; if attributable to the welfare rules, those couples migrate to the coupled constraint''s story and this story''s extraction drops accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(benefit_cliff_coupling_attribution, conceptual, 'Cross-constraint attribution of the marriage penalty at the registration gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__secular_contractual_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__secular_contractual_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__secular_contractual_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__secular_contractual_reading, theater_ratio, 50, 0.17).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__secular_contractual_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(fami_be_t10, family_law_authority__secular_contractual_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(fami_be_t20, family_law_authority__secular_contractual_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(fami_be_t30, family_law_authority__secular_contractual_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(fami_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(fami_be_t50, family_law_authority__secular_contractual_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(fami_be_t60, family_law_authority__secular_contractual_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fami_su_t10, family_law_authority__secular_contractual_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(fami_su_t20, family_law_authority__secular_contractual_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(fami_su_t30, family_law_authority__secular_contractual_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(fami_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(fami_su_t50, family_law_authority__secular_contractual_reading, suppression_requirement, 50, 0.46).
narrative_ontology:measurement(fami_su_t60, family_law_authority__secular_contractual_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, information_standard).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'marriage law': that label covers at least five structurally distinct validity regimes, authored as separate stories and linked through this network. This member is the state-exclusive criterion — no religious requirement, statutory gender symmetry, interfaith permitted, registration as the sole validity test. The sibling members retain religious constitutive elements and, in several cases, role-defined right allocations and union-boundary restrictions, so their victim sets and epsilon values differ structurally, not observationally; each sibling file carries its own epsilon over its own standing arrangement. Edges run from this reading to each sibling because the sole-criterion axiom, where it governs, renders rival validity sources legally inert within its jurisdiction — the foreclosure recorded in cs_structure.reading_relations. Pluralist polities segment jurisdictions (personal-law boards, opt-in civil marriage lanes) rather than resolving the contradiction; that segmentation is coexistence ACROSS frameworks and does not weaken the within-framework foreclosure this reading's axiom commits it to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
