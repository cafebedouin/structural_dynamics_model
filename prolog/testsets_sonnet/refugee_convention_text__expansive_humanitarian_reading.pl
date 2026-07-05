% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Expansive Humanitarian Reading of the Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention and 1967 Protocol define 'refugee' in
 *   language drafted for a specific mid-century context but applied globally
 *   for seven decades. This reading treats the text as a living humanitarian
 *   instrument: 'well-founded fear of persecution' extends to conditions of
 *   generalized violence and state collapse, not only individualized
 *   targeting; persecution by non-state actors (gangs, militias, clans,
 *   family members) counts where the state is unable or unwilling to protect;
 *   and 'particular social group' is read to include gender, sexual
 *   orientation and gender identity, and clan or lineage affiliation. Under
 *   this reading, interdiction at sea and offshore processing regimes that
 *   prevent claims from being heard are themselves violations of
 *   non-refoulement. UNHCR guidelines, much appellate jurisprudence in
 *   several jurisdictions, and a substantial scholarly consensus advance this
 *   reading; it is contested by states favoring narrower sovereign
 *   discretion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.28).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.35).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Expansive Humanitarian Reading of the Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '845df001-8988-42dc-9d07-f0addd0ff000').
narrative_ontology:cs_kernel_codification('845df001-8988-42dc-9d07-f0addd0ff000', fixed_text).
narrative_ontology:cs_authority_grounding('845df001-8988-42dc-9d07-f0addd0ff000', expertise).
narrative_ontology:cs_interpretation_layer_present('845df001-8988-42dc-9d07-f0addd0ff000').
narrative_ontology:cs_reading_relation('845df001-8988-42dc-9d07-f0addd0ff000', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('845df001-8988-42dc-9d07-f0addd0ff000', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('845df001-8988-42dc-9d07-f0addd0ff000', foundational, convention_as_evolving_humanitarian_instrument).
narrative_ontology:cs_axiom_status(convention_as_evolving_humanitarian_instrument, holdable).
narrative_ontology:cs_axiom_grounding('845df001-8988-42dc-9d07-f0addd0ff000', convention_as_evolving_humanitarian_instrument, deontological).
narrative_ontology:cs_axiom('845df001-8988-42dc-9d07-f0addd0ff000', foundational, non_state_persecution_qualifies_where_state_protection_absent).
narrative_ontology:cs_axiom_status(non_state_persecution_qualifies_where_state_protection_absent, holdable).
narrative_ontology:cs_axiom_grounding('845df001-8988-42dc-9d07-f0addd0ff000', non_state_persecution_qualifies_where_state_protection_absent, conventional).
narrative_ontology:cs_reference_frame('845df001-8988-42dc-9d07-f0addd0ff000', postwar_refugee_definition_1951).
narrative_ontology:cs_drift_state('845df001-8988-42dc-9d07-f0addd0ff000', contemporary_mixed_migration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('845df001-8988-42dc-9d07-f0addd0ff000', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_generalized_violence).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, gender_persecution_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, lgbtq_asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_state_asylum_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, border_and_interdiction_authorities).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_as_jus_cogens).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, living_instrument_treaty_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee conditions of civil war, gang control, or state collapse where no individualized targeting can be documented. Under this reading their claims are cognizable as well-founded fear even without a named persecutor singling them out; their survival depends on adjudicators accepting generalized violence as sufficient basis for protection.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_generalized_violence, beneficiary,
    powerless, biographical, trapped, global).

% Flee forced marriage, FGM, domestic violence regimes their states decline to prosecute, or gender-based social exclusion. This reading recognizes gender as constituting a particular social group; their protection depends entirely on that doctrinal move holding.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, gender_persecution_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Flee criminalization or vigilante violence tied to sexual orientation or gender identity, often from both state and non-state actors. This reading's inclusion of non-state persecution and broad social-group construction is the doctrinal basis their claims rest on.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, lgbtq_asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Flee inter-clan violence or exclusion in contexts of weak or absent state authority, where the persecutor is a militia, clan, or community rather than the state itself. Their claims require the non-state actor and social group doctrines this reading advances.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_claimants, beneficiary,
    powerless, biographical, trapped, regional).

% Advances this interpretation through guidelines, amicus interventions, and supervisory oversight under Article 35. Has an institutional and doctrinal stake in the Convention being read as a broad, evolving humanitarian instrument rather than a narrow sovereign concession, since a narrower reading would shrink its supervisory relevance and protection mandate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate, agenda_setter,
    institutional, generational, analytical, global).

% Must adjudicate a substantially larger and more contestable caseload under this reading, since generalized violence and diffuse social-group claims are harder to verify than individualized persecution. Bear the processing burden, backlog costs, and domestic political friction generated by broader eligibility criteria they did not choose to expand.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_state_asylum_agencies, payer,
    institutional, biographical, constrained, national).

% Operate interdiction and offshore processing regimes that this reading classifies as refoulement violations. Bear the compliance cost of dismantling or reforming those regimes, or the reputational and legal cost of maintaining them against the doctrine's weight.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, border_and_interdiction_authorities, payer,
    powerful, immediate, constrained, national).

% Bear the downstream fiscal and social-integration consequences of expanded eligibility and are not parties to the treaty-interpretation process that sets those consequences. Their preferences enter only indirectly, through elected governments that must operate within the interpretation international bodies and courts have endorsed.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, domestic_electorates_in_destination_states, excluded,
    organized, biographical, mobile, national).

% Reject the expansive reading as exceeding what they consented to at accession, but face reputational and legal costs for departing from it since UNHCR guidance and appellate case law increasingly treat it as the governing interpretation. Their sovereignty-floor reading is a live rival position, not a defeated one, but it operates at a structural disadvantage in international fora.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_states, excluded,
    institutional, generational, constrained, national).

% Adjudicate individual cases and, cumulatively, entrench or narrow the doctrinal scope of well-founded fear and particular social group through precedent. Their case-by-case rulings are the mechanism by which this reading either consolidates or erodes over time.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared international standard for who must be protected from return to danger, so that protection does not depend solely on each state's unilateral and potentially self-serving definition of persecution.
% TRANSFER_FUNCTION: Moves the burden of protection and processing from countries of origin (which have failed or actively caused the harm) to destination states and international institutions, and moves adjudicative discretion from state immigration agencies to a body of evolving international doctrine and jurisprudence.
% ABSENT_VOICES: Domestic electorates in destination states and legislatures that never explicitly voted to expand eligibility to generalized violence, gender, and non-state persecution claims are not parties to the treaty-interpretation process; restrictive_sovereignty_states object but operate at a doctrinal disadvantage in international fora and case law.
% DISAPPEARANCE_RATIONALE: If this reading were displaced by the restrictive_sovereignty_reading, a substantial share of currently-protected claimants — those fleeing generalized violence, non-state persecution, and social-group-based harms not tied to state action — would lose eligibility overnight; interdiction and offshore processing regimes currently contested as refoulement would become permissible; the asylum system's caseload composition and adjudicative standards would shift materially.
% FOUNDING_PROBLEM: The 1951 Convention was built to prevent the return of people to death or persecution after states had already demonstrated, through the Holocaust and postwar displacement, that individual states could not be trusted to define 'refugee' narrowly enough to serve their own interests when lives were at stake.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR, human rights tribunals, and refugee law scholars attest the founding problem persists and has expanded to new forms of persecution the drafters did not anticipate (gender, sexual orientation, state collapse), supporting evolutive interpretation. Restrictive_sovereignty_reading proponents, including several signatory states' foreign ministries, attest the founding problem was solved for the drafters' intended scope and that this reading extends the mandate beyond what any state consented to — that dispute is unresolved and not adjudicated by any party outside the interpretive contest itself.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because the reading's primary function is protective coordination — it moves protection burden toward those with capacity and moral obligation, not extraction from a captured population. Suppression is moderate (0.35) reflecting the active doctrinal and institutional work (UNHCR guidance, appellate precedent, treaty-body pressure) required to hold this reading against sovereign resistance; it is not self-enforcing. Theater ratio is low (0.22): the doctrinal machinery (guidelines, jurisprudence, supervisory review) substantially performs its stated protective function rather than substituting performance for substance, though some drift toward procedural box-ticking in mass-claim contexts is real. Accessibility collapse is low (0.3): the restrictive and procedural readings remain live, contested alternatives in international discourse, not eliminated. Resistance is high (0.68) because sovereign states actively contest this reading in courts, legislatures, and international fora — this is a doctrine that must be continuously defended, not one universally accepted.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers across all four expanded categories (generalized violence, gender, LGBTQ+, clan-based) are the clear beneficiaries — trapped, powerless agents whose survival depends on this reading's doctrinal scope. UNHCR sits as agenda_setter with an institutional stake in the mandate's breadth. Destination-state asylum agencies and interdiction authorities are payers: they bear the processing, compliance, and reform costs of a broader eligibility standard and a doctrine that classifies their existing offshore practices as violations, without having authored that doctrine themselves. No victim group is named because the reading's function is fundamentally protective rather than extractive from an identifiable population — the costs fall on state administrative capacity and sovereign discretion, not on a population from whom rents are extracted, which is why claimed_type is rope rather than tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — states defining 'refugee' narrowly enough to serve their own interests while people died — remains live by the corroboration of UNHCR and human rights tribunals, but is contested as already-answered-and-now-exceeded by restrictive sovereignty states. This divergence is exactly the six-questions mechanism's function: it prevents either collapsing the reading into pure extraction (it does solve a genuine, still-cited coordination and protection problem) or certifying it as costlessly settled (the founding-problem status is genuinely contested, not resolved, and the corroboration comes from parties who also benefit from the mandate's institutional continuation — UNHCR's own relevance depends partly on the mandate remaining broad).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the expansive_humanitarian_reading diverge structurally from its siblings, and is that divergence resolvable by evidence or only by normative commitment?',
    'Track how appellate courts and UNHCR supervisory practice resolve specific doctrinal flashpoints: (1) whether ''unable or unwilling'' state-protection tests for non-state persecution converge or diverge across jurisdictions over time; (2) whether social-group jurisprudence stabilizes around a shared test or remains split between immutability-only and broader constructions; (3) whether interdiction/offshore processing is authoritatively settled as refoulement or remains contested state practice.',
    'If jurisprudence converges toward the expansive reading, this reading becomes the de facto governing interpretation and the restrictive_sovereignty_reading''s suppression cost rises; if it converges toward the restrictive reading, this constraint''s accessibility_collapse would need to be revised upward on future measurement and its beneficiary population would face abrupt eligibility contraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'The location and resolvability of the disagreement between kernel readings.').

omega_variable(
    unhcr_institutional_interest_confound,
    'Does UNHCR''s institutional stake in a broad protection mandate (funding, mandate relevance, supervisory scope) bias its advocacy for this reading independent of the reading''s substantive merit?',
    'Compare UNHCR''s interpretive positions across periods of funding scarcity versus abundance, and against positions taken by protection actors with no comparable institutional stake (independent legal scholars, national human rights institutions with fixed mandates).',
    'If UNHCR advocacy tracks its own institutional interest more than independent legal reasoning, the founding_problem_corroboration weakens — UNHCR''s attestation of a live founding problem would be partially self-interested rather than purely evidentiary, though this would not by itself refute the reading''s substantive claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unhcr_institutional_interest_confound, conceptual, 'Whether the primary corroborating institution''s interest confounds its testimony.').

omega_variable(
    generalized_violence_verification_difficulty,
    'Does the practical unverifiability of generalized-violence and diffuse social-group claims (relative to individualized persecution) create a structural opening for either over-inclusion (protecting economic migrants under a humanitarian label) or under-inclusion (adjudicators defaulting to skepticism given verification difficulty)?',
    'Empirical study of grant-rate variance across adjudicators and jurisdictions for generalized-violence versus individualized-persecution claims, controlling for country-of-origin conditions.',
    'High variance would suggest the doctrine''s breadth, however normatively justified, produces inconsistent outcomes that function as a lottery rather than a stable standard — relevant to whether accessibility_collapse and resistance are accurately scored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalized_violence_verification_difficulty, empirical, 'Whether the broadened evidentiary standard produces consistent or arbitrary outcomes in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(refu_tr_t1985, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(refu_tr_t2000, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(refu_tr_t2025, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.12).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(refu_be_t1985, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(refu_be_t2000, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement(refu_be_t2025, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.15).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(refu_su_t1985, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1985, 0.22).
narrative_ontology:measurement(refu_su_t2000, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2000, 0.27).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(refu_su_t2025, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the refugee_convention_text kernel. expansive_humanitarian_reading (this file) authors a broad victim/beneficiary set and treats interdiction/offshore processing as refoulement violations, producing low-moderate extraction (0.28) and a rope classification driven by genuine protective coordination. restrictive_sovereignty_reading authors a narrow individualized-persecution standard and treats broad discretion as legitimate sovereign prerogative — expect a different beneficiary set (destination states, not claimants) and a different ε. procedural_integrity_reading is agnostic on substantive scope and scores process integrity independent of outcome breadth. The three files must each be evaluated on their own ε; none is a parameterization of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
