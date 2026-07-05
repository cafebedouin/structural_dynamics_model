% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Magna Carta (1215) as Feudal Baronial Contract — Protections Limited to Landholding Free Men
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the baronial-privilege reading of the Magna Carta
 *   kernel: the 1215 charter read as a feudal contract negotiated between
 *   King John and his rebelling tenants-in-chief, in which 'liber homo' (free
 *   man) designates the class of landholding barons and greater churchmen who
 *   were parties to the settlement, and the charter's protections — trial by
 *   peers, capped feudal incidents, due process before disseisin — extend
 *   only to that contracting class. Under this reading the charter has
 *   nothing to say about villeins, women, or the landless, because the
 *   13th-century legal category it deploys does not include them. This
 *   reading is one of three sibling constraints sharing the magna_carta_1215
 *   kernel; the universal_rights_reading treats 'free men' as proto-universal
 *   and Clause 39 as emitting a transhistorical due-process norm, while the
 *   living_document_reading treats the original 1215 meaning as legitimately
 *   superseded by centuries of interpretive accretion (Coke, the American
 *   founders, 20th-century jurisprudence). This story does NOT attempt to
 *   adjudicate between the readings or average their epsilon values — it
 *   generates the baronial-privilege reading alone, as ε-invariance requires.
 *
 * KEY AGENTS:
 *   - landholding_barons: primary beneficiary and co-agenda-setter (powerful/constrained) — forced the settlement and enforce it via the Security Council of Twenty-Five
 *   - king_john_and_successors: primary payer (institutional/trapped) — sovereign whose prerogatives are curtailed, repudiated the charter within weeks
 *   - unfree_villeins, landless_laborers, women_of_all_classes: excluded (powerless/trapped) — outside the 'liber homo' class this reading defines the protection set around
 *   - constitutional_historians: analytical observer — documents 1215 usage of 'liber homo' against later universalist retrofitting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.55).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.62).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) as Feudal Baronial Contract — Protections Limited to Landholding Free Men").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, 'd79330a1-111f-454a-83a9-0f3a4613d94a').
narrative_ontology:cs_kernel_codification('d79330a1-111f-454a-83a9-0f3a4613d94a', fixed_text).
narrative_ontology:cs_authority_grounding('d79330a1-111f-454a-83a9-0f3a4613d94a', lineage).
narrative_ontology:cs_interpretation_layer_present('d79330a1-111f-454a-83a9-0f3a4613d94a').
narrative_ontology:cs_reading_relation('d79330a1-111f-454a-83a9-0f3a4613d94a', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d79330a1-111f-454a-83a9-0f3a4613d94a', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('d79330a1-111f-454a-83a9-0f3a4613d94a', foundational, protection_bounded_to_contracting_tenurial_class).
narrative_ontology:cs_axiom_status(protection_bounded_to_contracting_tenurial_class, holdable).
narrative_ontology:cs_axiom_grounding('d79330a1-111f-454a-83a9-0f3a4613d94a', protection_bounded_to_contracting_tenurial_class, conventional).
narrative_ontology:cs_axiom('d79330a1-111f-454a-83a9-0f3a4613d94a', foundational, liber_homo_denotes_landholding_freeman_not_universal_person).
narrative_ontology:cs_axiom_status(liber_homo_denotes_landholding_freeman_not_universal_person, holdable).
narrative_ontology:cs_axiom_grounding('d79330a1-111f-454a-83a9-0f3a4613d94a', liber_homo_denotes_landholding_freeman_not_universal_person, empirically_contingent).
narrative_ontology:cs_reference_frame('d79330a1-111f-454a-83a9-0f3a4613d94a', runnymede_feudal_settlement_1215).
narrative_ontology:cs_drift_state('d79330a1-111f-454a-83a9-0f3a4613d94a', post_tenures_abolition_1660, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d79330a1-111f-454a-83a9-0f3a4613d94a', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landholding_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, great_ecclesiastical_lords).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, chartered_towns_elite).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, unfree_villeins).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women_of_all_classes).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, landless_laborers).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, the_king_as_constrained_sovereign).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john_and_successors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rebellious tenants-in-chief who forced the charter on King John at Runnymede to protect their feudal privileges — relief payments, wardship, scutage, and due process before their peers. They wrote the document's protections around their own class interest and enforce it through the security council of twenty-five barons empowered to seize royal property on breach. They cannot fully exit the relationship with the crown since their land tenure depends on it, but they hold the leverage of armed rebellion and did in fact use it again within a year.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landholding_barons, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landholding_barons, agenda_setter).

% Bishops and abbots holding land as tenants-in-chief who secured Clause 1's guarantee of the English Church's freedom and their own feudal protections alongside the lay barons. They benefit from the same contractual structure without bearing the risk of open rebellion, since their institutional standing gives them leverage independent of arms.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, great_ecclesiastical_lords, beneficiary,
    powerful, generational, constrained, national).

% The sovereign whose customary prerogatives over wardship, relief, scutage, and arbitrary justice against tenants-in-chief are curtailed by the charter. John repudiated it within weeks and sought papal annulment; the constraint's persistence depended entirely on baronial military capacity to re-impose it, which is exactly what happened in the First Barons' War. The king is trapped in the sense that his tenurial revenue base and legitimacy both depend on the same baronial class the charter constrains him against.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john_and_successors, payer,
    institutional, generational, trapped, national).

% The majority of the rural population, legally unfree and holding land only at their lord's will, appear in the charter only as chattel to be assessed for amercement 'saving his wainage' — a protection of the lord's property interest in their continued productivity, not a right held by them. They have no standing under 'liber homo' and no voice in the charter's drafting or its later invocation.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, unfree_villeins, excluded,
    powerless, biographical, trapped, local).

% Even baronial widows and heiresses, who do receive specific clauses regulating forced remarriage and dower, hold these as protections administered through male kin and feudal inheritance law rather than as autonomous legal personhood. Common women are entirely absent from the text's contemplation. Exit from the constraint's exclusions requires a change in the entire framework of feudal and marital property law, which is centuries away.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women_of_all_classes, excluded,
    powerless, biographical, trapped, national).

% Free by legal status in some cases but holding no land, they fall outside the tenurial relationships the charter regulates. The document's remedies — trial by peers, protection from disseisin, due process before judgment — presuppose a landholding stake that this group lacks, so the charter's machinery has nothing to attach to on their behalf.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landless_laborers, excluded,
    powerless, biographical, trapped, local).

% Read the 1215 text against its immediate context — a peace treaty between an insolvent, defeated king and a rebellious baronial coalition — and document that 'liber homo' in 1215 legal usage denoted the class of free tenants holding land directly or indirectly of the crown, not humanity in general. They note the later universalist readings are a genuinely distinct historical and legal development, not a discovery of original meaning.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the crown and its major tenants-in-chief: it fixes the terms of feudal incidents (relief, wardship, marriage, scutage) so that baronial families can plan succession and taxation exposure without arbitrary royal seizure, and it establishes a peer-review mechanism (trial by equals) so barons are not judged unilaterally by royal officials.
% TRANSFER_FUNCTION: Moves discretionary power away from the king and toward the baronial class: predictable, capped feudal payments replace arbitrary royal extraction from tenants-in-chief. Nothing moves toward villeins, women, or the landless — the charter reallocates power inside the class of free landholders, chiefly the barons, and leaves the class boundary itself untouched.
% ABSENT_VOICES: Villeins, landless free men, and women of every class would object that 'liber homo' excludes them from any of the charter's remedies, but none of them sat at Runnymede, none are named as parties, and the text gives them no procedural standing to invoke its clauses on their own behalf.
% DISAPPEARANCE_RATIONALE: If the 1215 charter's baronial-privilege function had never been enforced, the specific feudal incidents it capped (arbitrary relief, wardship abuse, scutage without consent) would have remained at the king's discretion, and the barons' capacity to check royal power outside of outright rebellion would not exist in the same institutionalized form. The relationship between crown and tenants-in-chief would rearrange around whatever alternative check (recurring rebellion, papal arbitration) filled the gap.
% FOUNDING_PROBLEM: King John's arbitrary and escalating extraction from his tenants-in-chief — inflated reliefs, abused wardships, unconsented scutage, judicial proceedings without peer trial — had made baronial tenure insecure enough that the barons organized militarily to force a negotiated settlement fixing the terms of the feudal relationship.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the beneficiary class (the barons and their descendants) attest that the specific feudal incidents the 1215 charter regulated — knight's fee relief schedules, wardship and marriage of tenants-in-chief, scutage by common counsel — ceased to be live legal or political problems centuries ago with the abolition of feudal tenure (notably the Tenures Abolition Act 1660). No party today has a live stake in relief-payment schedules; the charter's baronial-privilege function is genealogically dead even though later readings kept portions of the text (chiefly clauses 39-40) alive for a different purpose.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.55, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.55) reflects that this is a genuine bilateral bargain redistributing feudal-incident revenue between crown and baronage — a real transfer, but a bounded one between two elite parties, not a broad extraction from a wide victim population. Suppression (0.62) is set high because the charter's initial persistence depended entirely on the credible threat and actual exercise of baronial armed rebellion (the Security Council's distraint powers, the First Barons' War) — this is coercive machinery, not voluntary compliance. Theater ratio starts low (0.15, a live, contested, freshly fought-for arrangement) and rises steeply over the interval to 0.75 by 1660 as feudal tenure itself was phased out by economic and legal change (the actual incidents the charter regulated became progressively less operative) while symbolic invocation of 'Magna Carta' as a name grew — the baronial-privilege function decayed into inertial legal text even as the document's cultural authority (routed through the sibling readings) increased. Accessibility collapse (0.7) reflects that once you understand the 13th-century meaning of 'liber homo,' the narrow scope is fairly hard to argue away on textualist grounds — the historical record collapses alternative readings of what the 1215 drafters meant, even though the LATER readings legitimately construct different constraints from the same text. Resistance (0.45) is moderate: the king resisted hard (repudiation, papal annulment) but the barons prevailed by force; commoners, having no standing to resist an exclusion they were never inside of, register near-zero resistance under this specific reading.
 *
 * PERSPECTIVAL GAP:
 *   The baronial seat and the excluded seats would compute this constraint very differently if either could see it: from inside the baronial-privilege reading, the arrangement is a coordination success — a genuine, hard-won contractual fix to a real extraction problem between crown and tenants-in-chief (tangled_rope: coordination for the contracting class, extraction risk shifted onto the king's discretionary power). From the excluded seats, there is no seat at all — they are not parties, not victims of THIS constraint's operation in the sense of bearing its transfer, but structurally outside its protection while living under the same crown. Their exclusion is what makes this reading narrow rather than universal; it is not that they lose from the charter's operation so much as the charter never reaches them.
 *
 * DIRECTIONALITY LOGIC:
 *   Barons and great ecclesiastical lords are structural beneficiaries: they collect the settlement's protections (capped relief, protected wardship terms, peer trial) and co-administer enforcement via the Security Council — d sits near the beneficiary end, tempered by their constrained exit (their land tenure is what the constraint is about, so full exit means abandoning the entire feudal relationship). The king is the structural payer: prerogative curtailed, revenue capped, subject to distraint on breach — d sits near the target end, and he is genuinely trapped since his legitimacy and tenurial revenue depend on the same baronial class constraining him. Villeins, landless laborers, and women are marked 'victim' in base_properties not because the charter actively extracts FROM them under this reading, but because the charter's protective apparatus is unavailable to them while the tenurial order it stabilizes continues to govern their subordinate position — their victimhood here is exclusion-based rather than extraction-based, and an omega below flags this distinction.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading, the founding problem (arbitrary royal extraction from tenants-in-chief) is dead — feudal tenure and its incidents were abolished in 1660, so there is no live baronial-privilege function left to defend or reform. Reading the 1215 charter as if the baronial machinery still operates would be mandatrophy in the other direction: mistaking a historically bounded settlement for a living arrangement. This reading's classification (tangled_rope, historically) correctly separates the settled feudal-contract function, which is dead, from whatever ongoing constitutional function the sibling living_document_reading and universal_rights_reading claim the SAME TEXT performs today — those are different constraints with different founding problems, some of which may still be live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusion_vs_extraction_victim_status,
    'Are villeins, landless laborers, and women properly counted as ''victims'' of this constraint (people the constraint extracts from) or merely as ''excluded parties'' (people the constraint''s protective machinery does not reach, while their subordination is produced by other structures — manorialism, coverture — that this constraint neither creates nor remedies)?',
    'Compare the pre-1215 and post-1215 legal treatment of villein amercement and wardship of non-baronial land: if the charter''s existence measurably worsened their position (e.g., by legitimating a baronial class that then extracted more from villeins once its own tenure was secured), victim status is stronger; if their position is structurally unchanged by the charter''s presence or absence, exclusion is the more accurate characterization.',
    'If genuinely extraction rather than mere exclusion, the tangled_rope classification is reinforced (coordination for barons riding on identifiable extraction from villeins). If purely exclusion with no causal extraction, the constraint is closer to a rope for its contracting parties with a null relationship to the excluded groups, and the excluded groups arguably belong under a different constraint (the manorial system) rather than as victims here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_vs_extraction_victim_status, conceptual, 'Whether excluded non-parties should be coded as victims of this constraint or of a separate, uncharted manorial constraint.').

omega_variable(
    liber_homo_semantic_stability,
    'Is ''liber homo'' in 1215 usage a stable, well-bounded legal category (supporting this reading''s narrow scope as the historically correct one) or was its scope already contested and evolving even at the time of drafting, such that the later universalist reading has a legitimate textual foothold rather than being purely anachronistic retrofitting?',
    'Philological and legal-historical analysis of ''liber homo'' usage in contemporaneous 13th-century charters, writs, and legal treatises (e.g., Bracton) outside Magna Carta itself, to establish whether the term had a single settled referent or admitted contextual variation.',
    'If ''liber homo'' was already a contested or gradually widening category in 1215-era legal practice, the sharp baronial/universal reading split this story assumes is itself a later imposition, and the omega would need to be resolved toward acknowledging partial textual support for gradual, not purely anachronistic, expansion. If the term was tightly and uniformly bounded to tenurial freemen in contemporaneous usage, this reading''s narrow scope is more strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_semantic_stability, empirical, 'Whether 1215-era legal usage of ''free man'' was a settled narrow category or already semantically unstable.').

omega_variable(
    natural_vs_constructed_class_boundary,
    'Is the barons-only scope of protection a natural consequence of 13th-century feudal legal structure (an inevitable feature of any settlement negotiated between crown and tenants-in-chief) or a constructed choice the barons could have extended to a broader free population but chose not to, in order to preserve their own bargaining leverage and class distinction?',
    'Examine draft articles and negotiating history (the Articles of the Barons, contemporaneous chronicler accounts) for evidence of whether broader inclusion was considered and rejected, versus never contemplated as a live option given the legal categories available at the time.',
    'If broader inclusion was a live, considered, and rejected option, the narrow scope reads more as a constructed extraction-preserving choice by the barons; if the categories of the time made broader inclusion structurally unthinkable, the narrow scope reads more as an artifact of the era''s legal ontology rather than a deliberate exclusionary strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_class_boundary, conceptual, 'Whether the narrow beneficiary class was a deliberate baronial choice or a structural feature of 13th-century legal categories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1660).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.25).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).
narrative_ontology:measurement(magn_tr_t1297, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1297, 0.4).
narrative_ontology:measurement_basis(magn_tr_t1297, observed).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1400, 0.55).
narrative_ontology:measurement_basis(magn_tr_t1400, observed).
narrative_ontology:measurement(magn_tr_t1550, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1550, 0.65).
narrative_ontology:measurement_basis(magn_tr_t1550, observed).
narrative_ontology:measurement(magn_tr_t1660, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1660, 0.75).
narrative_ontology:measurement_basis(magn_tr_t1660, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.58).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.5).
narrative_ontology:measurement_basis(magn_be_t1225, observed).
narrative_ontology:measurement(magn_be_t1297, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1297, 0.45).
narrative_ontology:measurement_basis(magn_be_t1297, observed).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1400, 0.4).
narrative_ontology:measurement_basis(magn_be_t1400, observed).
narrative_ontology:measurement(magn_be_t1550, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1550, 0.35).
narrative_ontology:measurement_basis(magn_be_t1550, observed).
narrative_ontology:measurement(magn_be_t1660, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1660, 0.25).
narrative_ontology:measurement_basis(magn_be_t1660, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.6).
narrative_ontology:measurement_basis(magn_su_t1225, observed).
narrative_ontology:measurement(magn_su_t1297, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1297, 0.5).
narrative_ontology:measurement_basis(magn_su_t1297, observed).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1400, 0.35).
narrative_ontology:measurement_basis(magn_su_t1400, observed).
narrative_ontology:measurement(magn_su_t1550, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1550, 0.2).
narrative_ontology:measurement_basis(magn_su_t1550, observed).
narrative_ontology:measurement(magn_su_t1660, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1660, 0.1).
narrative_ontology:measurement_basis(magn_su_t1660, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is the baronial-privilege member of the magna_carta_1215 kernel family (3 stories). It shares a text and an origin event with universal_rights_reading and living_document_reading but instantiates a structurally distinct claim with a distinct victim/beneficiary set: here the protection set is bounded to landholding tenants-in-chief and excludes commoners, women, and the landless by construction. universal_rights_reading treats the same clause 39 language as emitting a transhistorical due-process norm covering all persons — a much wider beneficiary class and correspondingly different epsilon profile. living_document_reading treats present legitimacy as resting on accumulated interpretive tradition rather than either original meaning, and is downstream of both narrower readings in the sense that later interpreters had to first move past (or reinterpret) the baronial-privilege scope to arrive at broader applications. This story's founding_problem is DEAD (feudal tenure abolished 1660); the sibling readings' founding problems (arbitrary executive detention/process, and constitutional legitimacy across time respectively) may remain live — that determination belongs to those stories, not this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
