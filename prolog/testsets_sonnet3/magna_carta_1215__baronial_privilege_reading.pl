% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta (1215) as Feudal Contract Among King and Barons
 *   domain: constitutional law / legal history / political theory
 *
 * SUMMARY:
 *   This story instantiates the baronial-privilege reading of the Magna Carta
 *   kernel: the 1215 charter as a feudal contract negotiated at swordpoint
 *   between King John and his tenants-in-chief, in which 'liber homo' (free
 *   man) denotes landowning barons and higher clergy, and the protections
 *   against disseisin, arbitrary taxation, and denial of peer-judged justice
 *   run only between the crown and the contracting baronial class. On this
 *   reading the charter is a settlement of an intra-elite dispute, not a
 *   proto-constitutional bill of universal rights. Villeins, non-baronial
 *   free tenants, women, and townspeople outside the named chartered boroughs
 *   are structurally outside its protective scope — not merely unmentioned,
 *   but excluded by the document's own operative mechanism (peer judgment
 *   among equals, baronial enforcement council), which presupposes membership
 *   in the tenant-in-chief class. This is a narrow-scope reading; the sibling
 *   universal_rights_reading and living_document_reading (separate constraint
 *   stories) author very different beneficiary/victim sets and different
 *   epsilon values because they are reading different structural claims out
 *   of the same text — per the epsilon-invariance principle these are three
 *   constraints, not one constraint measured three ways.
 *
 * KEY AGENTS:
 *   - landowning_barons: Primary beneficiary and co-author (powerful/constrained) — extracts binding limits on royal power against their own class
 *   - higher_clergy: Co-beneficiary (organized/mobile) — negotiated parallel ecclesiastical liberty guarantees
 *   - king_john_and_successors: Primary payer (institutional/constrained) — coerced into ceding prerogative under military threat
 *   - villeins_and_serfs: Excluded (powerless/trapped) — outside the charter's rights-holder class entirely
 *   - women_of_all_classes: Excluded (powerless/trapped) — excluded by the gendered scope of 'liber homo'
 *   - free_tenant_farmers_without_baronial_status: Excluded (moderate/constrained) — free but outside the tenant-in-chief class the mechanism serves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.58).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.62).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) as Feudal Contract Among King and Barons").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional law / legal history / political theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, 'cc1641a7-abea-42f4-856e-baa1bbb9b213').
narrative_ontology:cs_kernel_codification('cc1641a7-abea-42f4-856e-baa1bbb9b213', fixed_text).
narrative_ontology:cs_authority_grounding('cc1641a7-abea-42f4-856e-baa1bbb9b213', lineage).
narrative_ontology:cs_interpretation_layer_present('cc1641a7-abea-42f4-856e-baa1bbb9b213').
narrative_ontology:cs_reading_relation('cc1641a7-abea-42f4-856e-baa1bbb9b213', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('cc1641a7-abea-42f4-856e-baa1bbb9b213', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('cc1641a7-abea-42f4-856e-baa1bbb9b213', foundational, liber_homo_denotes_tenant_in_chief_class).
narrative_ontology:cs_axiom_status(liber_homo_denotes_tenant_in_chief_class, holdable).
narrative_ontology:cs_axiom_grounding('cc1641a7-abea-42f4-856e-baa1bbb9b213', liber_homo_denotes_tenant_in_chief_class, empirically_contingent).
narrative_ontology:cs_axiom('cc1641a7-abea-42f4-856e-baa1bbb9b213', foundational, protection_scope_bounded_by_contracting_parties).
narrative_ontology:cs_axiom_status(protection_scope_bounded_by_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('cc1641a7-abea-42f4-856e-baa1bbb9b213', protection_scope_bounded_by_contracting_parties, conventional).
narrative_ontology:cs_reference_frame('cc1641a7-abea-42f4-856e-baa1bbb9b213', id_1215_baronial_feudal_settlement).
narrative_ontology:cs_drift_state('cc1641a7-abea-42f4-856e-baa1bbb9b213', post_1225_reissue_parliamentary_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cc1641a7-abea-42f4-856e-baa1bbb9b213', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, higher_clergy).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, villeins_and_serfs).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, free_tenant_farmers_without_baronial_status).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women_of_all_classes).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, urban_merchants_outside_charter_towns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john_and_successors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and militarily compelled acceptance of the charter at Runnymede to check King John's arbitrary taxation, disseisin, and justice-selling against their own class. The document's protections (Clause 39's 'liber homo', the scutage consent clause, the security council of twenty-five barons) are written for and secured by men holding land directly of the crown. They benefit from formalized limits on royal power exercised against them specifically, and from the enforcement mechanism (distraint of royal castles/lands) they control.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% Archbishop Langton and the senior bishops co-authored and are named first in the charter (Clause 1, church's freedom). They gain guaranteed freedom of ecclesiastical elections and property, negotiated as part of the same baronial settlement, and have exit options via ecclesiastical courts and papal appeal that lay barons lack.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, higher_clergy, beneficiary,
    organized, generational, mobile, national).

% Compelled under military threat to accept limits on prerogative — scutage by consent, no disseisin without lawful judgment of peers, security committee with distraint power. The king's exit was to reject and fight the ensuing civil war (which he did within weeks), or, for successors, to reissue a diluted version (1216, 1217, 1225) once the immediate coercive pressure eased.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john_and_successors, payer,
    institutional, civilizational, constrained, national).

% The unfree majority of the population, bound to manors and to their lord's court, not the king's. The charter's 'liber homo' language does not describe them; where the document mentions villeins at all (Clause 20, amercement limits) it treats them as an asset class whose 'wainage' (means of livelihood) must be preserved for the lord's benefit, not as rights-holders. They have no seat at Runnymede, no representative, and no mechanism by which the charter's protections reach them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, villeins_and_serfs, excluded,
    powerless, biographical, trapped, local).

% Legally free but holding no substantial land directly of the crown; they are neither the barons the charter protects nor the villeins it occasionally mentions. In the 1215 settlement's own terms they fall into an ambiguous middle the document does not clearly address, and in practice the charter's remedies (peer judgment, the baronial security council) are structured around and administered by the landed nobility, not accessible to them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, free_tenant_farmers_without_baronial_status, excluded,
    moderate, biographical, constrained, local).

% Excluded from 'liber homo' entirely by the term's gendered scope in 1215 usage and by feudal land tenure law; the charter's few clauses addressing women (dower rights, restrictions on compelled remarriage) treat them as objects of property arrangements between men, not as parties whose consent or judgment is secured by the peer-judgment clauses.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women_of_all_classes, excluded,
    powerless, biographical, trapped, local).

% London and a handful of towns received specific liberty guarantees (Clause 13) as part of the baronial coalition's bargaining, but merchants and townsmen outside those named charters gained nothing from the settlement — their trading liberties remained whatever royal or local lords chose to grant, unaffected by the peer-baron protections.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, urban_merchants_outside_charter_towns, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem among the tenants-in-chief: individually no baron could check the king's arbitrary disseisin, extortionate scutage, or sale of justice, but collectively armed and organized they extracted a written, sworn limit enforceable by a standing baronial committee with distraint power over royal assets.
% TRANSFER_FUNCTION: Moves discretionary power away from the king and toward the baronial class specifically — control over taxation consent, judgment by peers, and a formal enforcement council — while leaving the relationship between barons and everyone beneath them in the feudal hierarchy (their own tenants, villeins, women, urban non-elites) untouched or in some clauses reinforced (Clause 20's protection of a villein's 'wainage' is protection of the lord's asset, not the villein's person).
% ABSENT_VOICES: Villeins, free non-baronial tenants, women, and townspeople outside the chartered boroughs would have had grievances against both royal AND baronial exaction, but none were party to the negotiation at Runnymede and none appear as rights-holders in the operative clauses — their absence is structural, not incidental, since the charter's entire mechanism (peer judgment, baronial security council) is built around and by the landed elite.
% DISAPPEARANCE_RATIONALE: Had the 1215 settlement never existed or been permanently voided (as John attempted via papal annulment within weeks), the baronial class would have lost its primary written instrument for constraining scutage and disseisin, likely prolonging or intensifying the First Barons' War and altering the crown-baron balance of power for generations; the arrangement's disappearance would rearrange elite political structure while leaving villein, female, and non-baronial status largely unaffected either way.
% FOUNDING_PROBLEM: King John's arbitrary taxation (scutage and tallage without consent), seizure of baronial lands without lawful judgment, and sale of justice had made the tenants-in-chief's tenure and status insecure; the charter was built to bind the king to predictable, peer-adjudicated treatment of his direct feudal tenants.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the baronial-descent tradition (e.g., constitutional historians examining the 1215 text against the later 1225 reissue and Parliamentary rolls) attest that the specific king-baron feudal dispute the charter answered was resolved and superseded by later constitutional and parliamentary development within a few centuries; no institution today defends the 1215 baronial-privilege arrangement as a live operative problem-solver — its continued citation serves symbolic and universal-rights purposes distinct from its original narrow function, which corroborates that the founding problem, as narrowly construed here, is dead even though descendant readings keep the text alive for other purposes.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that within its own narrow scope the charter functions as coordination among barons and does genuinely constrain a real king who had been extorting his tenants-in-chief — but the same instrument simultaneously entrenches and formalizes the baronial class's own extractive position over everyone beneath them in the feudal hierarchy, since clauses like the Clause 20 amercement-limit protect a villein's productive capacity as the LORD's asset, not the villein's entitlement. Suppression (0.62) captures both the military coercion required to obtain King John's signature in 1215 and the structural suppression of any competing claim to 'liber homo' status by non-barons — the term's narrow scope is itself an act of definitional suppression, not merely a description. The 1216 spike in both suppression_requirement and theater_ratio reflects the papal annulment and reissue crisis, where the charter's enforcement mechanism collapsed into open warfare and the document briefly became more a rallying symbol (theater) than an operative instrument, before the 1217 and 1225 reissues restored a more modest, more theater-light operative form with reduced but real baronial privilege intact.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowning barons and higher clergy sit near the beneficiary end of directionality: they authored the constraint, administer its central enforcement mechanism (the 25-baron security council with distraint power), and it exists structurally to serve their class interest. King John and successors sit near the target end: institutional power notwithstanding, the charter specifically extracts concessions from the crown at baronial insistence, though the crown retains long-run institutional capacity that mobile individual victims lack. Villeins, women, non-baronial free tenants, and non-chartered urban merchants are excluded rather than targeted or benefited by this specific reading of the text — they are outside the constraint's structural scope altogether, which is the central point of the baronial-privilege reading: the constraint does not extract from them under this reading because it does not reach them; their trapped/constrained exit options and powerless/moderate power levels reflect their position in the broader feudal order, not this specific instrument.
 *
 * MANDATROPHY ANALYSIS:
 *   The baronial-privilege reading treats the charter's founding problem (crown overreach against tenants-in-chief specifically) as resolved by the 13th-century parliamentary and common-law developments that followed — the specific king-baron feudal dispute is dead. Reading this reading in isolation prevents mislabeling the 1215 document as a universal human-rights charter (that mislabeling is the work of the sibling universal_rights_reading, evaluated separately) while also preventing the opposite error of dismissing it as pure elite self-dealing with no coordination function: the charter did solve a genuine collective-action problem for the barons even as it left the rest of the social order's extraction structures untouched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liber_homo_scope_ambiguity,
    'Does the 1215 term ''liber homo'' (free man) as used in Clause 39 denote only landowning tenants-in-chief and their class, or a broader category of legally free persons that historians and later readings have expanded?',
    'Comparative philological and legal-historical analysis of contemporaneous usage of ''liber homo'' in other 1215-era feudal and manorial documents, cross-referenced against who actually invoked Clause 39 in 13th-century litigation.',
    'If contemporaneous usage supports the narrower baronial reading, this story''s beneficiary/victim scope is well-grounded; if it supports a broader reading even in 1215, this story''s exclusion of non-baronial free tenants may be too narrow and the universal_rights_reading''s ε for the founding moment (not later interpretation) would need revisiting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_scope_ambiguity, empirical, 'Contemporaneous scope of ''liber homo'' at the moment of drafting.').

omega_variable(
    reading_selection_and_kernel_indeterminacy,
    'Is there a single fact of the matter about which reading (baronial-privilege, universal-rights, or living-document) is the ''correct'' one for the 1215 kernel, or is the kernel genuinely under-determined such that all three are simultaneously defensible depending on the interpretive community?',
    'This is a conceptual/committer-frame question rather than an empirical one; it cannot be resolved by further historical evidence alone, since the disagreement is partly about what counts as the relevant interpretive community (1215 drafters'' intent vs. subsequent constitutional tradition vs. contemporary rights theory).',
    'If the kernel is genuinely under-determined, all three sibling readings should be maintained in the corpus as coexisting constraints rather than one being treated as displacing the others; this bears on how the network edges between the three sibling stories should be interpreted (coexistence vs. supersession).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_indeterminacy, conceptual, 'Whether the Magna Carta kernel has a single determinate reading or is irreducibly multi-valent.').

omega_variable(
    villein_partial_inclusion_ambiguity,
    'Clause 20 limits amercements on a villein''s ''wainage'' (means of livelihood) — does this constitute a minimal, incidental protection extending to villeins, or is it purely a property-protection for the lord who owns the villein''s labor and assets?',
    'Examination of subsequent manorial court records to see whether Clause 20 was ever invoked by or on behalf of villeins themselves, versus only by or on behalf of their lords.',
    'If villeins never benefited even incidentally, the baronial-privilege reading''s victim set is confirmed as fully excluded; if some incidental protection reached villeins in practice, the beneficiary/victim boundary in this story would need a partial-inclusion nuance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(villein_partial_inclusion_ambiguity, empirical, 'Whether Clause 20''s villein-protective language had any operative effect for villeins themselves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.35).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.15).
narrative_ontology:measurement(magn_tr_t1220, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1220, 0.18).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.62).
narrative_ontology:measurement(magn_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.55).
narrative_ontology:measurement(magn_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.5).
narrative_ontology:measurement(magn_be_t1220, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1220, 0.53).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.75).
narrative_ontology:measurement(magn_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.85).
narrative_ontology:measurement(magn_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.6).
narrative_ontology:measurement(magn_su_t1220, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1220, 0.55).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_1215 kernel, decomposed per the epsilon-invariance principle because the natural-language label 'Magna Carta' conflates structurally distinct claims about who 'free men' denotes and what scope of protection Clause 39 emits. baronial_privilege_reading (this story) authors a narrow beneficiary/victim set (barons and clergy benefit; villeins, women, non-baronial tenants, and non-chartered townspeople are excluded) and a moderate extractiveness (0.58) reflecting genuine intra-elite coordination riding on continued exclusion of the unfree majority. universal_rights_reading authors a maximally broad beneficiary set (all persons) and correspondingly different epsilon and victim structure appropriate to a transhistorical due-process claim. living_document_reading treats the original-meaning question as substantially moot and authors its own epsilon around interpretive-authority dynamics rather than 1215 scope. The three are linked via network edges rather than merged because merging them would violate DP-001 (constraint identity/epsilon-invariance): the observable used (contemporaneous scope vs. transhistorical scope vs. interpretive accumulation) changes the answer, which means these are different constraints sharing a textual ancestor, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
