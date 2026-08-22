% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Exclusion Read as Frankish Anachronism Wrongly Extended to Non-Frankish Successions
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This story instantiates the cognatic-reversion reading of the Salic
 *   prohibition kernel: the claim that Salic Law was a narrow 6th-century
 *   Frankish customary rule governing allodial land inheritance among Salian
 *   Franks, with no original bearing on royal succession and no jurisdiction
 *   whatsoever over non-Frankish territories. On this reading, its
 *   14th-century invocation to bar women from the French throne, and its
 *   still more tenuous later invocation in Iberian, Burgundian, and other
 *   successor-state disputes, represent a retroactive legal fiction
 *   manufactured to serve specific agnatic claimants — not the enforcement of
 *   a genuine ancient mandate. Under this reading, cognatic primogeniture
 *   (eldest child regardless of sex) is the legitimate default, and
 *   territorial integrity/local custom should override any imported
 *   agnatic-purity rule. This is one of three readings of the same kernel;
 *   the immutable_mandate_reading and sovereign_override_reading are separate
 *   constraint stories with their own ε values and structures.
 *
 * KEY AGENTS:
 *   - agnatic_male_claimants: Primary beneficiary (powerful/arbitrage) — inherit thrones/territory via the exclusion rule
 *   - female_heirs_and_their_lineages: Primary target (moderate/trapped) — bear the extraction, displaced from rightful cognatic claims
 *   - rival_dynastic_houses: Inter-institutional agenda-setter/beneficiary (institutional/arbitrage) — selectively invoke the rule as convenient
 *   - jurists_of_agnatic_courts: Institutional beneficiary/agenda-setter (institutional/identity_locked) — professionally bound to defending whichever reading serves their patron
 *   - constitutional_historians: Analytical observer (analytical/analytical) — traces the rule's genuine 6th-century scope versus its later extension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.58).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Exclusion Read as Frankish Anachronism Wrongly Extended to Non-Frankish Successions").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '67f670aa-640e-42fe-8ba7-3684aecdd6bc').
narrative_ontology:cs_kernel_codification('67f670aa-640e-42fe-8ba7-3684aecdd6bc', distributed).
narrative_ontology:cs_authority_grounding('67f670aa-640e-42fe-8ba7-3684aecdd6bc', lineage).
narrative_ontology:cs_interpretation_layer_present('67f670aa-640e-42fe-8ba7-3684aecdd6bc').
narrative_ontology:cs_reading_relation('67f670aa-640e-42fe-8ba7-3684aecdd6bc', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('67f670aa-640e-42fe-8ba7-3684aecdd6bc', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('67f670aa-640e-42fe-8ba7-3684aecdd6bc', foundational, jurisdictional_scope_limits_customary_law).
narrative_ontology:cs_axiom_status(jurisdictional_scope_limits_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('67f670aa-640e-42fe-8ba7-3684aecdd6bc', jurisdictional_scope_limits_customary_law, empirically_contingent).
narrative_ontology:cs_axiom('67f670aa-640e-42fe-8ba7-3684aecdd6bc', secondary, cognatic_primogeniture_as_default_absent_valid_local_adoption).
narrative_ontology:cs_axiom_status(cognatic_primogeniture_as_default_absent_valid_local_adoption, holdable).
narrative_ontology:cs_axiom_grounding('67f670aa-640e-42fe-8ba7-3684aecdd6bc', cognatic_primogeniture_as_default_absent_valid_local_adoption, conventional).
narrative_ontology:cs_reference_frame('67f670aa-640e-42fe-8ba7-3684aecdd6bc', narrow_frankish_customary_land_law).
narrative_ontology:cs_drift_state('67f670aa-640e-42fe-8ba7-3684aecdd6bc', post_14th_century_succession_crises, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('67f670aa-640e-42fe-8ba7-3684aecdd6bc', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_male_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, rival_dynastic_houses).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, jurists_of_agnatic_courts).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_heirs_and_their_lineages).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, territories_of_originally_cognatic_custom).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, annexed_non_frankish_successor_states).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_over_agnatic_purity).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male collateral relatives who invoke the Salic exclusion to bypass closer female or female-descended claimants. They gain the throne, territory, or inheritance precisely because the exclusion rule is imported wholesale into jurisdictions where it never organically applied, converting a Frankish tribal custom into a continent-wide veto on female succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_male_claimants, beneficiary,
    powerful, generational, arbitrage, continental).

% Eldest daughters and their children who hold the strongest hereditary claim under the cognatic customs native to their own territory, but are displaced by agnatic male relatives citing Salic Law — a rule with no genealogical connection to their polity's own succession tradition. Their exit options are war, litigation before courts controlled by the beneficiaries, or acceptance of disinheritance.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs_and_their_lineages, payer,
    moderate, biographical, trapped, continental).

% Regions and kingdoms (outside the Frankish heartland) whose own indigenous succession customs permitted female or cognatic inheritance long before Salic Law was retroactively asserted over them. They bear the cost of having a foreign legal fiction imposed to settle disputes in their favor of outside claimants, eroding local sovereignty over succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territories_of_originally_cognatic_custom, payer,
    moderate, generational, constrained, regional).

% Territories absorbed into successor kingdoms whose rulers invoke Salic Law to consolidate agnatic control over inherited or conquered lands, even though these territories were never part of the original Frankish legal community and had no voice in adopting the rule.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, annexed_non_frankish_successor_states, payer,
    powerless, generational, trapped, national).

% Competing royal houses that selectively invoke or disclaim Salic Law depending on which reading favors their preferred claimant — asserting the anachronism argument when a female heir favors them, and asserting Salic force when an agnatic male favors them. They administer courts, convene estates, and issue legal opinions that decide which reading prevails in a given succession crisis.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, rival_dynastic_houses, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, rival_dynastic_houses, agenda_setter).

% Court lawyers and legal scholars whose professional standing rests on producing genealogical and doctrinal justifications for whichever succession outcome their patron favors. Their careers and the prestige of the legal tradition itself are bound up in defending the applicability (or, in this reading, the inapplicability) of the rule.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, jurists_of_agnatic_courts, beneficiary,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, jurists_of_agnatic_courts, agenda_setter).

% Scholars who trace Salic Law's origin to the 6th-century Lex Salica, a Frankish tribal code addressing land inheritance among free Salian Franks, and note its 14th-century retroactive invocation to exclude women from the French throne — then observe its further, still less grounded extension to wholly separate territories and successor states that never adopted Frankish customary law.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_male_claimants).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its genuine original scope, Salic Law solved a real coordination problem for Frankish landholding communities: fixing predictable rules for allodial land transmission among male kin to prevent fragmentation of holdings and recurring intra-family war over inheritance.
% TRANSFER_FUNCTION: As extended beyond that original scope, the rule transfers thrones, territories, and inheritance rights from female heirs and cognatic-custom territories to agnatic male claimants and the dynastic houses and jurists who benefit from asserting the rule's universal force.
% ABSENT_VOICES: The female heirs displaced by the extended rule, and the populations of annexed non-Frankish territories, are rarely parties to the legal proceedings that adjudicate succession — decisions are made by rival houses and their courts, with the excluded claimant's own subjects having no standing to contest the genealogical fiction.
% DISAPPEARANCE_RATIONALE: If the extended (non-Frankish) application of Salic exclusion were abandoned, numerous historical and prospective successions would revert to cognatic primogeniture: female heirs and their lineages would inherit crowns and territories currently held or claimed under agnatic-only rules, altering dynastic maps and legitimating claims currently treated as extinguished.
% FOUNDING_PROBLEM: The original Lex Salica addressed inheritance of allodial land among Salian Frankish kin groups in the 6th century — a narrow, geographically and ethnically bounded customary rule with no stated application to royal succession or to non-Frankish peoples.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the beneficiary houses and courts (e.g., legal historians tracing the 1316–1328 French succession crises and the later invocation against Iberian, Burgundian, and Habsburg successions) attest that the original land-inheritance rationale had no bearing on royal succession and no jurisdictional claim over non-Frankish polities — the rule was retroactively theorized centuries after its origin, precisely when it served the interests of specific agnatic claimants.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from near zero (0.1) at the rule's genuine 6th-century origin — when it was a locally coordinating land-inheritance custom — to substantial (0.62) by the interval's end, tracking its retroactive extension into royal succession law (14th century) and subsequent projection onto wholly separate, non-Frankish successor territories. Suppression tracks the same curve: enforcement required essentially none when the rule applied only within its native community, but rises sharply (to 0.58) once agnatic claimants needed courts, wars, and treaties to make the extended rule stick against contrary local custom and against female claimants with the stronger native-law claim. Theater ratio is moderate-high (0.45): a substantial share of the legal apparatus that invokes 'Salic Law' in these extended disputes is genealogical theater — constructing lineage narratives to justify outcomes decided on power grounds — rather than genuine application of a coherent ancient rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male claimants and the dynastic houses/jurists who back them sit near the full-beneficiary end: they gain thrones and territory and can shop between readings of the kernel depending on which favors them (arbitrage exit). Female heirs and cognatic-custom territories sit near the full-target end: their claims are the ones extinguished by the rule's imported application, and they have essentially no exit — they cannot simply leave the succession dispute or relitigate in a neutral forum, since the courts adjudicating the claim are typically controlled by the rival house asserting the exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (allodial land-transmission coordination among 6th-century Salian Franks) is dead by any genealogical accounting outside the beneficiary courts, yet the exclusion is still invoked centuries later and territories away, purely as leverage in succession disputes. This mismatch — status=dead paired against disappearance_verdict=world_rearranges — is exactly the zombie-mandate signature the framework is built to catch: the rule persists not because its coordination function survives, but because specific agnatic claimants have captured the genealogical fiction and use it as cover for what is, in this reading, straightforward extraction of inheritance rights from cognatic-favored claimants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frankish_jurisdictional_boundary_ambiguity,
    'Where, precisely, does ''Frankish jurisdiction'' end for purposes of Salic Law''s genuine original applicability — and did any non-Frankish polity ever voluntarily adopt it as customary law, as opposed to having it imposed by conquest or dynastic union?',
    'Comparative legal-historical analysis of surviving charters, customary law compilations, and succession precedents in each contested territory prior to the disputed invocation, checking for independent adoption versus externally imposed application.',
    'If some territories independently adopted agnatic-preference customs resembling Salic exclusion prior to any Frankish contact, the anachronism claim weakens for those territories specifically, narrowing this reading''s victim set; if no such independent adoption is found anywhere outside the Frankish heartland, the anachronism reading is strengthened across the board.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frankish_jurisdictional_boundary_ambiguity, empirical, 'Uncertainty over the genuine territorial boundary of Salic Law''s organic (non-imposed) applicability.').

omega_variable(
    kernel_framing_which_reading_is_the_default,
    'Is the cognatic-reversion reading itself a later constructed counter-narrative (arising to favor specific female or cognatic claimants) in the same way the immutable_mandate_reading is alleged to be a later constructed pro-agnatic narrative — or does one reading have a stronger claim to represent the rule''s genuine original scope?',
    'Philological and legal-historical dating of when each reading (anachronism vs. immutable mandate vs. sovereign-override) first appears in the documentary record, relative to the succession disputes each reading was first invoked to resolve.',
    'If the cognatic-reversion reading itself emerges only in the 19th-20th centuries as retrospective legal history serving cognatic claimants'' later interests, this reading''s own claim to represent the ''true'' original scope is weakened, and its ε and beneficiary structure should be understood as equally contested rather than epistemically privileged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_which_reading_is_the_default, conceptual, 'Whether this reading''s genealogical account is itself a constructed narrative rather than a neutral historical recovery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sali_tr_t120, salic_prohibition__cognatic_reversion_reading, theater_ratio, 120, 0.3).
narrative_ontology:measurement(sali_tr_t250, salic_prohibition__cognatic_reversion_reading, theater_ratio, 250, 0.42).
narrative_ontology:measurement(sali_tr_t400, salic_prohibition__cognatic_reversion_reading, theater_ratio, 400, 0.45).
narrative_ontology:measurement(sali_tr_t550, salic_prohibition__cognatic_reversion_reading, theater_ratio, 550, 0.44).
narrative_ontology:measurement(sali_tr_t700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 700, 0.45).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sali_be_t120, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 120, 0.35).
narrative_ontology:measurement(sali_be_t250, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 250, 0.55).
narrative_ontology:measurement(sali_be_t400, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(sali_be_t550, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 550, 0.6).
narrative_ontology:measurement(sali_be_t700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 700, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sali_su_t120, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 120, 0.4).
narrative_ontology:measurement(sali_su_t250, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 250, 0.55).
narrative_ontology:measurement(sali_su_t400, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(sali_su_t550, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 550, 0.58).
narrative_ontology:measurement(sali_su_t700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 700, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language label 'Salic Law' per the ε-invariance principle: this story (cognatic_reversion_reading, tangled_rope, ε=0.62 — genuine narrow coordination function corrupted by extraterritorial/extra-temporal extension) is linked to immutable_mandate_reading (which would claim near-mountain naturalness for the same rule) and sovereign_override_reading (which frames the rule as ordinary revocable positive law). The three share a kernel but diverge sharply on claimed_type and beneficiary structure because they disagree about what kind of rule Salic Law actually is — that disagreement is the kernel contest itself, modeled here as three linked constraints rather than one story with a hedged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
