% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Magna Carta 1215 as Feudal Contract Between King and Baronage (Baronial Privilege Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story reads the 1215 charter strictly as its own drafters and
 *   immediate context frame it: a feudal contract negotiated between an armed
 *   baronial faction and a besieged king, in which 'liber homo' (free man)
 *   refers to the class of free tenants — practically, the tenant-in-chief
 *   baronage and lesser free tenants with standing — and the protections
 *   against arbitrary disseisin, unlawful amercement, and denial of judgment
 *   by peers run only to parties within that contracting relationship.
 *   Villeins, the majority of the rural population, free peasants without
 *   baronial standing, urban commoners beyond municipal charter
 *   confirmations, women in their own right, and Jewish creditors (named
 *   adversely, not protectively, in clauses 10-11) fall outside the charter's
 *   operative scope under this reading. Extraction here is measured as the
 *   ratio of who bears the costs and exclusions of the settlement
 *   (unprotected classes, whose prior vulnerability to arbitrary lordly power
 *   is left wholly undisturbed, plus Jewish creditors who are textually
 *   disadvantaged) against who captures its protective machinery (the
 *   baronage). This is a tangled rope, not a snare: it possesses a genuine
 *   coordination function (constraining the king's arbitrary fiscal and
 *   judicial power against a specific class of tenants) alongside asymmetric
 *   extraction (the protective machinery is built, funded, and enforced by
 *   and for that class alone, while the rest of the population's status is
 *   untouched or actively worsened).
 *
 * KEY AGENTS:
 *   - landowning_barons: Primary beneficiary and co-drafter (powerful/arbitrage) — secures feudal protections and taxation consent
 *   - king_john_and_successors: Constrained party and enforcer (institutional/constrained) — bears the extraction of reduced discretion, administers the settlement
 *   - unfree_villeins: Excluded majority (powerless/trapped) — receives no standing under 'free men'
 *   - jewish_moneylenders: Named adversely (powerless/trapped) — textually disadvantaged rather than protected
 *   - legal_historians: Analytical observer — reconstructs contemporary meaning of liber homo from primary sources
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
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 as Feudal Contract Between King and Baronage (Baronial Privilege Reading)").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '6ff1fde0-eaed-4695-a0be-7c9cdad390ba').
narrative_ontology:cs_kernel_codification('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', fixed_text).
narrative_ontology:cs_authority_grounding('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', lineage).
narrative_ontology:cs_interpretation_layer_present('6ff1fde0-eaed-4695-a0be-7c9cdad390ba').
narrative_ontology:cs_reading_relation('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', foundational, liber_homo_denotes_feudal_tenant_class).
narrative_ontology:cs_axiom_status(liber_homo_denotes_feudal_tenant_class, holdable).
narrative_ontology:cs_axiom_grounding('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', liber_homo_denotes_feudal_tenant_class, empirically_contingent).
narrative_ontology:cs_axiom('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', foundational, charter_obligations_bind_only_contracting_parties).
narrative_ontology:cs_axiom_status(charter_obligations_bind_only_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', charter_obligations_bind_only_contracting_parties, conventional).
narrative_ontology:cs_reference_frame('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', feudal_baronial_contract_1215).
narrative_ontology:cs_drift_state('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', post_1225_reissue_statute_incorporation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ff1fde0-eaed-4695-a0be-7c9cdad390ba', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, great_ecclesiastical_lords).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, unfree_villeins).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, free_peasant_tenants).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, urban_commoners).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women_of_all_classes).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, jewish_moneylenders).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, baronial_consent_to_taxation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and forced the charter on King John at Runnymede after military rebellion. Secure their own feudal privileges — protection from arbitrary disseisin, scutage only by common counsel, judgment by peers — as a class of tenants-in-chief holding land directly of the crown. Can withdraw military and financial support from the king (and did) as leverage; hold castles and armed retinues as exit/enforcement capacity no other group possesses.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% Bishops and abbots holding baronies benefit alongside lay barons from the same clauses protecting free tenure and freedom of the Church (clause 1). Possess independent international leverage through Rome and can excommunicate or interdict, giving them exit options unavailable to lesser parties.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, great_ecclesiastical_lords, beneficiary,
    powerful, generational, arbitrage, national).

% Forced to seal the charter under military duress and sought papal annulment within weeks; his successors reissued modified versions repeatedly. The king administers the crown's relationship to the baronage under the charter's terms and is the party whose arbitrary action (disseisin, unlawful scutage, denial of justice to peers) the document is built to constrain — but only for the contracting class.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john_and_successors, agenda_setter,
    institutional, civilizational, constrained, national).

% Constitute the large majority of the rural population, bound to the land and to a lord's manorial court. The charter's protections run to 'free men' (liberi homines) — a term of art excluding villeins by definition. Clause 20's amercement protections for villeins' 'wainage' (implements of husbandry) are the rare exception, protecting the baron's productive asset rather than the villein as a rights-bearer. Have no voice in the charter's negotiation and no standing under it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, unfree_villeins, excluded,
    powerless, biographical, trapped, local).

% Technically 'free' in feudal-legal terms but hold no substantial land and no baronial standing. In practice cannot invoke judgment by peers in any court that matters, cannot bargain with the crown, and are subject to the same lords the charter empowers. The charter's language could in principle be read to cover them, but no enforcement mechanism, court, or advocate exists to make that reading operative for this class in 1215.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, free_peasant_tenants, excluded,
    powerless, biographical, trapped, local).

% London and other towns receive limited liberties (clause 13 confirms London's customs) but the mass of urban tradespeople and laborers are not parties to the negotiation and have no representation in the drafting baronial council. Benefit incidentally from municipal charter confirmations but not from the personal-liberty clauses.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, urban_commoners, excluded,
    powerless, biographical, constrained, local).

% Excluded from feudal tenancy-in-chief and thus from the charter's operative class of 'free men' regardless of rank; clauses on widows (7, 8) grant procedural protection against forced remarriage and dower deprivation, but these run to widows as an incident of their late husbands' tenures, not as rights held in their own name. No woman is a contracting party.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women_of_all_classes, excluded,
    powerless, biographical, trapped, national).

% Named adversely in clauses 10 and 11, which restrict debts owed to Jewish creditors upon a debtor's death or minority — protecting baronial heirs' estates from Jewish creditors specifically, singling them out rather than including them among protected parties. Bear a direct textual cost from a charter that names them only to their disadvantage.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, jewish_moneylenders, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, jewish_moneylenders, payer).

% Examine the 1215 text, the barons' Articles of the Barons, and contemporary chronicle evidence to establish what 'liber homo' meant to the drafters and how narrowly enforcement actually ran in the years immediately following sealing.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem among the tenant-in-chief baronage: constrains King John's arbitrary use of relief, scutage, disseisin, and denial of justice against fellow barons, replacing unilateral royal discretion with common counsel and judgment by peers for the baronial class.
% TRANSFER_FUNCTION: Moves discretionary power over taxation, feudal incidents, and judicial process from the king to the baronage as a bloc; imposes no corresponding transfer toward villeins, free peasants, urban commoners, women, or Jewish creditors, two of whom (clauses 10-11) are named only as sources of restricted claims against baronial estates.
% ABSENT_VOICES: Villeins, free peasants without baronial standing, urban laborers, women of every class, and Jewish moneylenders were not present at Runnymede and are not addressed by the charter's protective clauses as bearers of the rights it creates; their objection — that 'free men' functions as a term of exclusion rather than universal reference — is visible in the text's own vocabulary but was not voiced by any party with standing in 1215.
% DISAPPEARANCE_RATIONALE: If the 1215 charter (under this reading, as a baronial feudal contract) had never been sealed or been immediately voided without trace, the baronage would have lost its primary written instrument constraining royal fiscal and judicial discretion, and the specific mechanism of judgment by peers and common counsel on scutage would not exist as a precedent the baronage could invoke in subsequent conflicts with the crown (1216, 1217, 1225 reissues). The unprotected classes' situation would be materially unchanged, which is itself part of what this reading asserts.
% FOUNDING_PROBLEM: King John's escalating, arbitrary extraction from his tenants-in-chief — unpredictable relief payments, seizure of land without judgment, denial of access to royal courts, and heavy scutage to fund failed continental wars — had made baronial tenure insecure and provoked armed rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chroniclers (Roger of Wendover, Matthez Paris writing shortly after) attest the rebellion's fiscal and feudal grievances from outside the baronial council itself, and papal correspondence (Innocent III's bull annulling the charter within ten weeks) independently corroborates that the instrument was read at the time as a narrow baronial restraint on the crown, not a universal rights charter — the founding problem (unconstrained royal extraction from tenants-in-chief) was substantially resolved by subsequent constitutional development (Parliament, common law courts) centuries ago, though this reading holds the original document itself addressed only the baronial class.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) is moderate-high: the charter does real coordination work for its contracting class but the benefit set is narrow relative to the total population governed by the same crown, and two named groups (Jewish creditors, indirectly villeins under their lords) bear costs the instrument does not offset. Suppression (0.62) reflects the coercive character of the settlement's origin (armed rebellion, forced sealing, subsequent papal annulment and renewed war) and the fact that maintaining the baronial reading required repeated military and political contest through 1215-1217. Theater ratio is low (0.2) because the coordination function is substantively real for the contracting class, not merely performative — barons genuinely obtained enforceable constraints on relief and disseisin. Accessibility collapse is high (0.7): once the feudal-contract frame is understood, there was no accessible alternative reading available to excluded groups in 1215 — no court, advocate, or textual hook let a villein or woman invoke the charter on their own behalf.
 *
 * DIRECTIONALITY LOGIC:
 *   Barons and great ecclesiastical lords are structural beneficiaries: they co-authored the terms, hold military and financial leverage against the king, and the protective clauses are drafted around their specific feudal incidents (relief, wardship, scutage, disseisin of freehold). The king is the target of the extraction under this reading — his prior discretionary power is what the charter removes — but he retains institutional power and negotiates modified reissues, so his directionality is dampened relative to a fully powerless target. The excluded groups (villeins, free peasants, urban commoners, women, Jewish creditors) are not parties to the transfer at all under this reading; they are outside the constraint's directional axis entirely for the protective clauses, except where the text names them adversely (clauses 10-11), which gives Jewish moneylenders an active victim directionality distinct from the merely-excluded groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The baronial-privilege reading prevents the anachronistic move of treating the 1215 text as already accomplishing universal due process (that is a distinct constraint — the living-document and universal-rights readings — not this one). It also prevents flattening the charter into pure extraction: the coordination function among the baronage was real and effective (it produced a durable check on scutage and disseisin that outlasted John's reign through the 1216, 1217, and 1225 reissues). Classifying this reading as tangled_rope rather than snare or rope avoids both errors: snare would deny any genuine coordination occurred; rope would deny that the settlement's benefits were captured by a narrow class at the expense of everyone else's unchanged (or worsened) position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liber_homo_semantic_scope_1215,
    'Did ''liber homo'' in the 1215 text denote only tenant-in-chief barons and greater free tenants, or did contemporary usage already extend more broadly to a wider free (non-villein) population including some urban freemen?',
    'Philological analysis of contemporary legal usage of ''liber homo'' in plea rolls, charters, and the Assize of Clarendon-era documents contemporaneous with 1215, cross-referenced against the Articles of the Barons'' drafting history and the identity of the twenty-five sureties named in clause 61 (all barons).',
    'If contemporary usage was already broader than the baronial class, this reading''s victim set (free peasants, urban commoners) shrinks and some reclassification toward a wider rope-like coordination is warranted; if usage was as narrow as this reading claims, the tangled_rope classification with a narrow beneficiary set is strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_semantic_scope_1215, empirical, 'Whether 1215-contemporary legal usage of ''free man'' was as narrow as the baronial reading claims.').

omega_variable(
    villein_incidental_protection_extent,
    'Does clause 20''s protection of villeins'' wainage from disproportionate amercement constitute a genuine (if narrow) extension of protection to unfree tenants, undermining the claim that the charter''s protective machinery runs exclusively to the baronial class?',
    'Compare enforcement records (where any exist) of clause 20 invoked on behalf of villeins versus invoked to protect a lord''s economic interest in his villein''s productive capacity.',
    'If clause 20 was enforced as protecting the lord''s asset rather than the villein as rights-holder, it corroborates this reading''s exclusion of villeins from the protected class; if enforced occasionally in the villein''s own interest, the victim/beneficiary boundary is less absolute than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(villein_incidental_protection_extent, empirical, 'Whether clause 20 constitutes real villein protection or protection of a baronial economic asset.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct unit of analysis the 1215 sealed text alone (supporting a narrow baronial reading) or the 1215 text read together with its near-immediate reissues (1216, 1217, 1225), which already show some broadening of enforceable scope through Parliamentary confirmation practice?',
    'Textual comparison of the 1215, 1216, 1217, and 1225 versions for scope-relevant changes, and examination of whether reissue confirmations were sought or invoked by any non-baronial party in that decade.',
    'If the 1225 reissue (the version that became the basis for later statute-book inclusion) already shows meaningful broadening, this reading''s ε and victim set are more defensible as describing 1215 specifically but less defensible as characterizing ''Magna Carta'' as it entered enduring legal memory — this is exactly the kind of observable-dependent divergence the ε-invariance principle requires resolving by decomposition rather than averaging, and is part of why this kernel is split into three sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the 1215 text alone or the 1215-1225 reissue sequence is the correct referent for the baronial reading''s ε.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.15).
narrative_ontology:measurement_basis(magn_tr_t1216, observed).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.18).
narrative_ontology:measurement_basis(magn_tr_t1217, observed).
narrative_ontology:measurement(magn_tr_t1220, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1220, 0.19).
narrative_ontology:measurement_basis(magn_tr_t1220, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.2).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.62).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.58).
narrative_ontology:measurement_basis(magn_be_t1216, observed).
narrative_ontology:measurement(magn_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.55).
narrative_ontology:measurement_basis(magn_be_t1217, observed).
narrative_ontology:measurement(magn_be_t1220, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1220, 0.57).
narrative_ontology:measurement_basis(magn_be_t1220, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.58).
narrative_ontology:measurement_basis(magn_be_t1225, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.75).
narrative_ontology:measurement_basis(magn_su_t1216, observed).
narrative_ontology:measurement(magn_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.65).
narrative_ontology:measurement_basis(magn_su_t1217, observed).
narrative_ontology:measurement(magn_su_t1220, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1220, 0.6).
narrative_ontology:measurement_basis(magn_su_t1220, observed).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.62).
narrative_ontology:measurement_basis(magn_su_t1225, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_1215 kernel. baronial_privilege_reading (this story) authors ε=0.58 for a tangled_rope: genuine coordination among the baronial class plus asymmetric extraction against excluded classes, narrowly scoped to 1215-1225. universal_rights_reading authors a distinct, lower-extraction constraint premised on 'free men' reaching all persons and Clause 39 emitting a transhistorical due-process norm. living_document_reading authors a constraint premised on legitimate interpretive supersession of original meaning through centuries of precedent, with its own distinct ε reflecting the modern constitutional-substrate function rather than the 1215 feudal-contract function. The three do not average into one ε; each is a separate constraint linked here for contamination-propagation and family-tracking purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
