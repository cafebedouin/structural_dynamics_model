% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Religious-Zionist Reading: Divine Covenant Grounding Inalienable Territorial Claim
 *   domain: political_philosophy/religious_nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This story authors ONE reading among five of a contested kernel — the
 *   legitimacy structure of Jewish sovereignty over the land historically
 *   called Palestine/Eretz Yisrael. The religious-Zionist reading holds that
 *   a divine covenant, understood as textually and theologically established,
 *   grants the Jewish people an inalienable and non-negotiable title to the
 *   land, and that political sovereignty realizes rather than merely pursues
 *   that title. This reading is authored on its own terms: Palestinian claims
 *   are not weighed and found wanting within it, they are structurally
 *   outside its ledger, which is itself the key structural fact this story
 *   records. This is emphatically not a claim about which reading is correct
 *   — it is a structural account of what this reading's own logic does when
 *   instantiated as an operating constraint on settlement policy, land
 *   allocation, and political coalition-building.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.8).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Religious-Zionist Reading: Divine Covenant Grounding Inalienable Territorial Claim").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/religious_nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '1b6266eb-0e3e-4aca-a7cc-62360ed822bf').
narrative_ontology:cs_kernel_codification('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', fixed_text).
narrative_ontology:cs_authority_grounding('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', lineage).
narrative_ontology:cs_interpretation_layer_present('1b6266eb-0e3e-4aca-a7cc-62360ed822bf').
narrative_ontology:cs_reading_relation('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', foundational, divine_title_is_operative_legal_category).
narrative_ontology:cs_axiom_status(divine_title_is_operative_legal_category, holdable).
narrative_ontology:cs_axiom_grounding('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', divine_title_is_operative_legal_category, theological).
narrative_ontology:cs_axiom('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', foundational, territorial_boundary_is_theologically_fixed_and_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_boundary_is_theologically_fixed_and_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', territorial_boundary_is_theologically_fixed_and_non_negotiable, theological).
narrative_ontology:cs_axiom('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', secondary, political_sovereignty_is_religious_fulfillment_not_merely_pragmatic_refuge).
narrative_ontology:cs_axiom_status(political_sovereignty_is_religious_fulfillment_not_merely_pragmatic_refuge, holdable).
narrative_ontology:cs_axiom_grounding('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', political_sovereignty_is_religious_fulfillment_not_merely_pragmatic_refuge, theological).
narrative_ontology:cs_reference_frame('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', abrahamic_covenant_land_grant).
narrative_ontology:cs_drift_state('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', post_1967_settlement_era, gap(revival_pressure, severe, false)).
narrative_ontology:cs_created_at('1b6266eb-0e3e-4aca-a7cc-62360ed822bf', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, settlement_movement_leadership).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_of_west_bank).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, non_orthodox_jewish_dissenters_from_territorial_maximalism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the biblical land grant to the patriarchs as an active, unrevoked title held collectively by the Jewish people. Settles and expands presence in the West Bank (Judea and Samaria in this reading's own vocabulary) as an act of religious fulfillment, not political convenience. Regards withdrawal from any part of the promised land as theologically impermissible regardless of security or diplomatic cost; identity as a Jew in this reading is bound to the land claim itself, making exit from the position equivalent to abandoning covenant obligation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers, agenda_setter).

% Yeshivot, rabbinic councils, and settlement organizations that articulate and transmit the theological doctrine, train the next generation of settlers, and lobby the state to treat religious claims as overriding diplomatic or legal considerations. They administer the doctrine's continuity and benefit from state subsidy, land allocation, and political influence that flows from the claim's persistence.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Political and organizational figures who convert the theological claim into land acquisition, infrastructure funding, and electoral leverage. Unlike rank-and-file settlers whose identity is fused to the land, leadership retains more mobility and could, in principle, moderate the claim — but doing so would cost them the movement's core constituency and resource base.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, settlement_movement_leadership, beneficiary,
    organized, generational, mobile, regional).

% Live on land the reading declares divinely and permanently allocated to the Jewish people, which forecloses any political process that could recognize their own claims as co-equal or negotiable. Face land expropriation, movement restriction, and settlement expansion justified by a title they had no part in establishing and cannot contest within the reading's own terms, since the claim is asserted as theologically prior to and superior to any competing territorial or political argument.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_of_west_bank, payer,
    powerless, biographical, trapped, local).

% Displaced populations and descendants whose return or restitution claims are rendered moot by a reading that treats the entirety of the land as belonging exclusively to the covenant community. This reading does not weigh their claims within its own calculus at all — they are structurally absent from the framework's beneficiary/victim ledger except as an obstacle to fulfillment.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Secular, liberal, or non-Orthodox Jewish citizens and diaspora members who reject the theological framing but are politically and socially pressured by its dominance in coalition politics, settlement funding priorities, and religious authority over personal status law. Their exit is constrained by shared national identity and family/communal ties, not identity-locked in the same absolute sense as covenant settlers.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, non_orthodox_jewish_dissenters_from_territorial_maximalism, payer,
    moderate, biographical, constrained, national).

% Incorporates religious-Zionist doctrine into settlement policy, military deployment, and legal frameworks to varying degrees depending on governing coalition. Benefits from the doctrine's mobilizing power and demographic facts-on-the-ground it creates, while also bearing diplomatic and security costs the doctrine imposes; its exit from accommodating the claim is constrained by coalition dependency on religious-Zionist parties.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus, beneficiary).

% UN bodies, international courts, and treaty frameworks that assess occupation and settlement activity under international law. Their determinations (e.g., on settlement illegality) are treated by this reading as categorically inapplicable, since divine title is asserted as prior to and unreviewable by any secular legal order — they are structurally excluded from having standing within the reading's own logic, not merely disagreed with.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_community, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the covenant community with a unifying, transgenerational rationale for settlement, resistance to territorial compromise, and political mobilization — coordinating dispersed religious-nationalist actors around a single non-negotiable claim that resists erosion by shifting diplomatic circumstances.
% TRANSFER_FUNCTION: Moves land, water, and infrastructure access from Palestinian residents and refugees to the settler and settlement-institution beneficiaries, and moves political capital and state resources from the general Israeli citizenry (including dissenting Jewish citizens) toward settlement expansion and religious-authority institutions.
% ABSENT_VOICES: Palestinian residents, refugees, and their political representatives have no standing within this reading's own framework — their claims are not weighed and lost, they are structurally never entered into the calculus. International legal bodies are treated as illegitimate arbiters. Even non-Orthodox Jewish citizens who reject territorial maximalism are marginalized within domestic religious-nationalist coalition politics.
% DISAPPEARANCE_RATIONALE: If the theological claim lost its mobilizing force overnight, settlement expansion would lose its primary justificatory engine, coalition politics dependent on religious-Zionist parties would restructure, land negotiations could proceed on grounds other than divine title, and Palestinian residents of the West Bank would face a fundamentally different legal and political environment — the settlement enterprise's institutional and demographic momentum would not vanish, but its claimed inalienability would.
% FOUNDING_PROBLEM: Modern religious Zionism arose to answer whether Jewish return to and sovereignty over the land after millennia of exile and, later, after the Holocaust, could be understood as religiously mandated and theologically meaningful rather than merely a secular nationalist or pragmatic refuge project — it sought to give covenantal weight to what liberal and cultural Zionism treated as a political or cultural claim.
% FOUNDING_PROBLEM_CORROBORATION: Religious-Zionist rabbinic authorities and settlement leadership attest the covenantal claim remains fully live and unconditioned by circumstance. Secular Israeli historians, international legal scholars, and Palestinian testimony from outside the benefiting community attest that the doctrine has shifted from a post-1967 theological response into a durable instrument for territorial acquisition and political leverage — a function distinct from, and outliving, its original meaning-making purpose.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.88 at interval end) because the reading treats the entirety of the claimed land as non-negotiable, which as an operating political doctrine translates into continuous land acquisition, denial of partition legitimacy, and structural exclusion of competing claims from consideration — there is no ceiling on the claim's territorial demand internal to the doctrine itself. Suppression is high (0.8) because sustaining the claim against contrary international law, competing national claims, and even internal Jewish dissent requires active enforcement: military presence, legal argumentation, and political mobilization. Accessibility collapse is authored moderate rather than extreme (0.35) because, unlike a genuine mountain, real alternatives (partition, civic equality frameworks, cultural Zionism) are visibly available and actively argued by sibling readings — the collapse is asserted by the reading, not structurally total. Resistance is very high (0.9) reflecting sustained Palestinian, international, and intra-Jewish political resistance to the claim's operationalization.
 *
 * DIRECTIONALITY LOGIC:
 *   The covenant community and religious-Zionist institutions sit at the beneficiary end: they collect land, state subsidy, and political standing, and their identity is structurally fused with the claim (identity_locked exit — abandoning the land claim would mean abandoning covenantal Jewish identity as this reading defines it). Palestinian residents and refugees sit at the full-target end: trapped exit, no standing within the reading's own calculus, and direct territorial and legal costs. Non-Orthodox Jewish dissenters occupy an intermediate position — constrained rather than trapped, bearing political and social costs of the doctrine's dominance without being its intended targets. The Israeli state apparatus is dual-positioned: agenda-setter administering the doctrine's political weight, but also bearing diplomatic and security costs it did not choose unilaterally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving covenantal meaning to Jewish return after exile and catastrophe — is treated by the reading's own adherents as permanently live (a covenant does not expire). Corroboration from outside the benefiting community, however, indicates the doctrine has shifted function: from theological meaning-making to a durable instrument for territorial acquisition and political leverage that persists independent of, and sometimes against, Israeli state security or diplomatic interests. This mismatch (status: contested, but corroborated drift toward instrumentalization) is exactly the kind of signal the founding-problem interview is designed to surface rather than resolve — the framework records the divergence without adjudicating whether the covenant is theologically 'real.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_claim_political_instrument_ambiguity,
    'Is the divine-covenant claim best modeled as a sincere, internally coherent theological commitment that happens to have severe territorial implications, or as a political instrument that deploys theological language to secure land and resources that could not be justified on secular grounds alone?',
    'Comparative analysis of religious-Zionist doctrine''s flexibility under changed political circumstances (e.g., willingness to accept negotiated boundaries when politically advantageous vs. doctrinal rigidity) and tracing which actors materially benefit from the claim''s persistence versus which are purely ideologically committed.',
    'If primarily sincere theological commitment, the constraint is better modeled as an identity/meaning structure with extraction as an unintended structural byproduct; if primarily instrumentalized, the extraction is the point and theology is cover, strengthening the tangled_rope/snare-leaning classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_claim_political_instrument_ambiguity, conceptual, 'Whether the covenant claim is sincere theology with extractive byproducts or political instrumentalization using theological cover.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with the sibling readings originate — is it a disagreement about facts (was there a divine promise, is it still binding), about authority (who gets to interpret the promise''s political implications), or about scope (does a valid religious claim translate into an exclusive political sovereignty claim)?',
    'None available in principle — this is a disagreement about theological authority and political philosophy that does not resolve through empirical inquiry; the disagreement is genuinely located at the level of foundational axioms (divine title as a real, operative legal category) that the liberal-nationalist and settler-colonial readings do not share.',
    'Locating the disagreement at the axiom level (rather than at the level of contestable facts) means no shared evidentiary standard could adjudicate between this reading and its siblings — they are not competing empirical hypotheses but competing normative-theological frameworks, which is why the CS structure below treats several sibling relations as coexists_with rather than resolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'The disagreement between readings is located at the axiom level, not the factual level.').

omega_variable(
    palestinian_exclusion_reading_internal_or_external_critique,
    'Is the exclusion of Palestinian claims from this reading''s calculus a critique made from OUTSIDE the reading (by liberal-nationalist, settler-colonial, or international-legal observers) or is it also recognized and contested from WITHIN religious-Zionist thought itself (e.g., minority rabbinic voices arguing covenant obligations include ethical treatment of non-Jewish residents)?',
    'Survey of internal religious-Zionist theological literature and rabbinic responsa for minority positions that argue the covenant is compatible with Palestinian rights or civic equality, versus mainstream settlement-movement doctrine.',
    'If internal dissent exists and is marginalized, this reading is better modeled as one contested pole within religious Zionism rather than the unanimous position of the tradition, which would refine (without changing) the beneficiary/victim ledger authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_exclusion_reading_internal_or_external_critique, empirical, 'Whether Palestinian exclusion is contested even within religious-Zionist thought or only from outside it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(jewi_tr_t55, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 55, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(jewi_be_t55, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 55, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(jewi_su_t55, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 55, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five decomposed readings of the jewish_sovereignty_palestine kernel, each authored as a structurally distinct constraint with its own ε per the ε-invariance principle. The religious_zionist_reading carries the highest authored extractiveness in the family (0.88) because its core axiom (divine, inalienable title) admits no negotiated partition and structurally excludes Palestinian claims from its own ledger — a delta the liberal_nationalist_reading (self-determination compatible with partition) and cultural_zionist_reading (sovereignty-optional) do not share. The settler_colonial_reading shares this story's high extractiveness reading of outcomes but attributes it to structural colonial dynamics rather than theological entitlement — the two readings converge on severity while diverging entirely on mechanism and legitimacy. The post_zionist_reading is downstream of all sovereignty-claiming readings, since it presupposes statehood was achieved and interrogates what that achievement now forecloses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
