% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Clause 39 as Bounded Prohibition on Documented Royal Abuses (Originalist Limitation Reading)
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the magna_carta_clause_39 kernel:
 *   the originalist_limitation_reading, under which clause 39 is a bounded
 *   prohibition covering only the royal abuses documented in the 1215 context
 *   — arbitrary disseisin, imprisonment without lawful judgment, punitive
 *   reliefs and scutage, abused wardship — as itemized in the Articles of the
 *   Barons. On this reading the constraint's protected class is the 1215
 *   category 'free man', its target is the crown's documented abusive
 *   practices, and its normative force does not extend past the settlement's
 *   own frame. The eps referent is the standing arrangement under contest —
 *   clause 39's operation as a limit on royal power — assessed by this
 *   reading's own lights: a real, remedially motivated, actively enforced
 *   restraint with moderate bite. KEY AGENTS (by structural relationship):
 *   anglo_norman_crown — primary target (institutional/constrained), bears
 *   the constraint's extraction and later administers parts of it;
 *   rebel_baronial_coalition — agenda-setter and principal beneficiary
 *   (organized/constrained), wrote, imposed, and enforced the prohibitions
 *   protecting itself; community_of_free_men_1215 — protected beneficiary
 *   class (moderate/trapped); english_church — institutional beneficiary via
 *   the opening-clause alliance (institutional/constrained); villein_tenantry
 *   — excluded voice (powerless/trapped), outside 'free man'; papacy —
 *   external adjudicating observer (institutional/analytical). Per the
 *   eps-invariance principle, the colloquial label 'Magna Carta clause 39'
 *   decomposes into three structurally distinct constraints — this reading
 *   and the liberal_due_process and feudal_prerogative siblings — each with
 *   its own eps, victim set, and temporal reach; this file authors only its
 *   own reading and links the family through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - anglo_norman_crown: Primary target (institutional/constrained) — surrenders the documented abusive prerogatives; bears the constraint's costs; administers its judicial enforcement after 1217
 *   - rebel_baronial_coalition: Agenda-setter and principal beneficiary (organized/constrained) — drafted, imposed, and enforced the charter; collects the protection it wrote
 *   - community_of_free_men_1215: Protected beneficiary class (moderate/trapped) — receives tenure and bodily security without negotiating power
 *   - english_church: Institutional beneficiary (institutional/constrained) — secures ecclesiastical freedoms and mediates the settlement
 *   - villein_tenantry: Excluded voice (powerless/trapped) — suffers parallel abuses with no protection and no seat
 *   - papacy: Analytical observer (institutional/analytical) — external tribunal; annuls the charter in 1215
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.42).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.5).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 as Bounded Prohibition on Documented Royal Abuses (Originalist Limitation Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'c436248b-248e-4f52-90d4-0a9ee1b2455c').
narrative_ontology:cs_kernel_codification('c436248b-248e-4f52-90d4-0a9ee1b2455c', fixed_text).
narrative_ontology:cs_authority_grounding('c436248b-248e-4f52-90d4-0a9ee1b2455c', lineage).
narrative_ontology:cs_interpretation_layer_present('c436248b-248e-4f52-90d4-0a9ee1b2455c').
narrative_ontology:cs_reading_relation('c436248b-248e-4f52-90d4-0a9ee1b2455c', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('c436248b-248e-4f52-90d4-0a9ee1b2455c', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('c436248b-248e-4f52-90d4-0a9ee1b2455c', foundational, clause_scope_bounded_to_documented_1215_grievances).
narrative_ontology:cs_axiom_status(clause_scope_bounded_to_documented_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('c436248b-248e-4f52-90d4-0a9ee1b2455c', clause_scope_bounded_to_documented_1215_grievances, empirically_contingent).
narrative_ontology:cs_axiom('c436248b-248e-4f52-90d4-0a9ee1b2455c', foundational, grant_context_fixes_normative_force).
narrative_ontology:cs_axiom_status(grant_context_fixes_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('c436248b-248e-4f52-90d4-0a9ee1b2455c', grant_context_fixes_normative_force, conventional).
narrative_ontology:cs_reference_frame('c436248b-248e-4f52-90d4-0a9ee1b2455c', runnymede_documented_grievance_baseline).
narrative_ontology:cs_drift_state('c436248b-248e-4f52-90d4-0a9ee1b2455c', contemporary_expansionist_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c436248b-248e-4f52-90d4-0a9ee1b2455c', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, rebel_baronial_coalition).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, community_of_free_men_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, english_church).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, anglo_norman_crown).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, law_of_the_land_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, judgment_of_peers_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grants the charter under armed duress and thereby surrenders specific prerogatives: arbitrary disseisin, imprisonment without lawful judgment, punitive reliefs and scutage as practiced against the 1215 grievance list. Bears the constraint's costs directly — every prohibition is a subtraction from discretionary royal power. After the 1216-1217 reissues the crown also administers the constraint through its own sheriffs and justices, so the paying seat runs parts of the enforcement machinery it submits to. Exit looks like civil war (attempted 1215-1217, failed at the cost of the dynasty's position) or papal appeal (obtained, then overtaken by John's death); leaving the legal order entirely is not available.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, anglo_norman_crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, anglo_norman_crown, agenda_setter).

% Drafts the Articles of the Barons, negotiates at Runnymede, imposes the charter, and enforces it through clause 61's committee of twenty-five with power of distraint. The same men who wrote the prohibitions are their principal protected class: their disseised lands are restored, their reliefs capped, their heirs shielded from punitive wardship. Their exit option was the war they had already started; after the settlement their position depends on the charter holding.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, rebel_baronial_coalition, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, rebel_baronial_coalition, beneficiary).

% The class the words 'free man' actually reached in 1215: barons, knights, and free tenants holding by charter or fee. They receive security of tenure and bodily security against the enumerated abuses without having negotiated anything. They are bound to their tenures and lordships; exit from the feudal order is not a live option, so their protection arrives attached to the land they cannot leave.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, community_of_free_men_1215, beneficiary,
    moderate, generational, trapped, national).

% Secures its freedoms in the charter's opening clause and supplies the mediating figure (Archbishop Langton) around whom the baronial coalition coheres. Gains guaranteed ecclesiastical elections and freedom from royal interference as part of the same settlement. Its position inside the realm makes exit meaningless; its leverage comes from canon-law legitimacy and the threat of interdict.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, english_church, beneficiary,
    institutional, generational, constrained, national).

% The majority of the population, unfree tenants on manorial estates. They suffer the same categories of abuse the charter prohibits — seizures, arbitrary levies, corporal control — but the words 'free man' do not reach them, and no one represented their interests at Runnymede. They would object that the settlement purchases security for one estate of the realm while leaving the rest under exactly the arbitrary power the barons just curtailed for themselves. Exit is not available to them at all.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, villein_tenantry, excluded,
    powerless, generational, trapped, regional).

% Adjudicates the settlement's legitimacy from outside the realm: declares the charter void as extorted and derogating from royal dignity (August 1215), suspends Langton, and licenses John's counter-campaign. Takes no material flow from the constraint either way; its seat is the external tribunal whose rulings the parties must reckon with.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, papacy, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, rebel_baronial_coalition).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__originalist_limitation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts the king-baron feud cycle into a public, negotiated boundary: 'lawful judgment of his equals or the law of the land' replaces unilateral seizure as the operating rule for royal action against free men, giving crown and subjects a predictable interface (courts, due process, capped incidents) instead of recurring private war.
% TRANSFER_FUNCTION: Moves prerogative discretion from the crown to procedural constraint: security of tenure and of person shifts from revocable royal favor to presumptive entitlement for free men. Concretely, the crown surrenders arbitrary disseisin, discretionary imprisonment, and punitive fiscal incidents against the documented grievance list, while the baronial class acquires enforceable protection and the remedial machinery (clause 61's twenty-five) to recover what was taken.
% ABSENT_VOICES: Villein tenantry — the majority of the population — stand outside 'free man' and therefore outside the clause's protection entirely; unfree tenants endured the same categories of abuse with no seat at Runnymede. Women other than widows and heiresses, and town communities below baronial rank, are likewise unrepresented. Present, they would object that the settlement prices security for one estate of the realm while leaving everyone else under the arbitrary power it just curtailed for the protected class.
% DISAPPEARANCE_RATIONALE: Overnight removal returns royal action against free men to unilateral discretion: the tenure-security and due-process expectations built on the settlement collapse back into favor, the remedial machinery dissolves, and the entire downstream lineage of confirmations, statutory due process, and constitutional argument that claims descent from Runnymede loses its anchor.
% FOUNDING_PROBLEM: King John's documented abuses against his barons and free tenants: arbitrary disseisin of lands, imprisonment without lawful judgment (the Briouze starvations the notorious extreme), punitive reliefs and scutage, and exploitation of wardship and marriage — grievances itemized in the Articles of the Barons and answered by clause 39's enumerated prohibitions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Close and Patent Rolls record the disseisins, amercements, and fiscal incidents; monastic chroniclers hostile to John but independent of the baronial negotiators (Roger of Wendover, Matthew Paris) document the abuses, including the Briouze deaths; modern archival historians (J.C. Holt, David Carpenter) reconstruct the grievance list from the pipe rolls and the negotiation drafts. Baronial self-attestation alone would be cover-story risk; the external administrative and chronicle record carries the genealogy.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the constraint's bite against the crown is real but bounded: it removes the documented abusive practices while leaving ordinary prerogative, ordinary justice, and ordinary revenue intact — the settlement was remedial, not confiscatory. Suppression (0.50) is structural, not internalized: the constraint originally rested on military occupation of London and clause 61's distraint apparatus, then migrated into judicial institutionalization; no seat internalizes the arrangement as natural. Theater (0.30) is low-to-moderate and rising slowly — the charter was operative throughout the window, but the rhythm of reign-opening confirmations begins converting part of its maintenance into ritual by the 1240s-1260s. Accessibility_collapse (0.50): alternatives did not fully close — the crown renegotiated, litigated at Rome, and fought; the barons' alternative was the war they had already waged — so the constraint narrowed the option space without eliminating it. Resistance (0.70) is high: immediate papal annulment, the First Barons' War, and repeated reissue under duress. The measurement series run on one shared time grid (years since 1215: 0, 2, 10, 20, 30, 40, 50, 60) with every tracked metric authored at every point. The extractiveness and suppression series show a mild crisis-driven oscillation across 1235-1265 (royal encroachment, baronial reassertion, settlement) — a side effect of reign politics, not an intermittent-reinforcement mechanism; the suppression series is authored because this story specifically tracks enforcement-capacity change (clause 61 apparatus falling away, judicial machinery replacing it, crisis renewals ratcheting it back up).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the drafter-enforcer seats should compute differently, and the structural data forces it. From the crown's position the charter is a coerced confiscation of legitimate regality — imposed at sword-point, annulled by the pope, and only tolerable because the dynasty's survival required acquiescence; the crown's own administrators then operate the machinery, a payer-administering-its-own-restraint position with no analogue among the beneficiaries. From the baronial coalition's position the same text is the minimum price of peace and the restoration of what was unlawfully taken. The villein seat sits outside both experiences: the text neither restrains nor protects anyone in that position, which is the structural fact the excluded role records. The engine computes these per-seat classifications from power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. anglo_norman_crown is the sole declared victim and the constraint's cost-bearer with constrained exit (war or compliance; the papal exit failed) — it derives near the full-target end. rebel_baronial_coalition combines agenda_setter with beneficiary: it wrote the rules, runs the enforcement committee, and is the rules' chief protected class — capture-grade positioning that derives very near the beneficiary end despite its enforcement role. community_of_free_men_1215 benefits without agenda power and is tenure-trapped — low directionality but thinner capture than the negotiators. english_church benefits through the opening-clause alliance — low directionality. villein_tenantry is deliberately NOT declared as beneficiary or victim: exclusion from the text's protected category is a structural absence, recorded as the excluded stakeholder role and routed to Q4, not manufactured into a directionality value. papacy holds the analytical seat. No directionality_overrides are needed: the derivation from roles, power, and exit produces the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading makes the genealogy explicit, and the R5 mismatch flag is expected and informative here: founding_problem_status=dead (the enumerated 1215 abuses were remediated and did not recur in that form within the window) combined with disappearance_verdict=world_rearranges (the constitutional order rearranges around the charter) yields the zombie/capture signal. The honest resolution under THIS reading: the instrument's mandate completed, and its continuing career is carried by successor interpretive regimes — the sibling readings this file links but does not instantiate. Within the bounded frame the constraint is not theatrically maintained (theater 0.30, modest, and mostly functional through 1265), so the flag does not indicate a piton here; it indicates that the constraint's persistence beyond its mandate belongs to the sibling stories' books, not this one. Classification discipline cuts both ways: coding the 1215 instrument as a snare would misread a negotiated, remedial, bounded restraint as pure extraction; coding it as a mountain would misread a constructed, resisted, twice-renegotiated settlement as natural law. Tangled_rope is the structurally true claim: genuine coordination function (averting permanent civil war, installing a public legal boundary), asymmetric extraction (the crown pays in prerogative; the negotiating class collects protection), and active enforcement throughout (clause 61, war, reissue, judicial institutionalization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the magna_carta_clause_39 kernel — the originalist_limitation_reading. Which reading governs a given evaluation, and how much of this story''s classification transfers across readings?',
    'Evaluation contexts must declare their reading; cross-reading comparison routes through the network edges linking the three family members, never through this story''s metrics or victim set.',
    'Sibling readings instantiate different constraints: liberal_due_process_reading expands the protected class to all persons facing arbitrary state power and raises epsilon accordingly; feudal_prerogative_reading narrows to procedural formality within hierarchy. None of this story''s values apply to them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; siblings are separate files, not hedges inside this one.').

omega_variable(
    scope_of_application_disagreement_location,
    'Where exactly is the inter-reading disagreement located — in the definition of the protected class (''free man''), in the enumeration of cognizable abuses, or in the temporal reach of the clause''s normative force?',
    'Comparative alignment of the three reading-stories'' beneficiary/victim declarations and temporal intervals, locating the structural element on which they actually diverge.',
    'If the dispute is located in temporal reach, this reading differs from its siblings chiefly on obsolescence and the R5 mismatch flag is the whole delta; if in protected class, the victim sets differ categorically and cross-reading epsilon comparison is invalid rather than merely divergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_application_disagreement_location, conceptual, 'Locates the axis along which the three readings of the clause are structurally incomparable.').

omega_variable(
    documented_abuse_enumeration_completeness,
    'Is the set of royal abuses ''documented in the 1215 context'' determinate enough to fix this reading''s constraint boundary — do the Articles of the Barons, the Unknown Charter, and the Close and Patent Rolls jointly enumerate the cognizable abuses?',
    'Archival reconstruction cross-referencing the negotiation documents against the recorded disseisins, imprisonments, and fiscal incidents of roughly 1204-1215 in the pipe and patent rolls.',
    'Gaps in the record widen or narrow the bounded scope, moving epsilon within the moderate band; severe indeterminacy would push this reading toward the feudal_prerogative sibling''s thinner procedural frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_abuse_enumeration_completeness, empirical, 'Whether the documentary record fixes the boundary this reading claims for the clause.').

omega_variable(
    security_clause_separability,
    'Does the clause 61 enforcement apparatus (the committee of twenty-five, distraint, right of levy) belong to this constraint''s structure, or is it a separable mechanism whose removal in the 1216 reissue changed the constraint''s identity?',
    'Compare the suppression trajectory before and after the 1216-1217 reissues: if judicial institutionalization substituted for the distraint apparatus while preserving the constraint''s function, the apparatus is separable; if function lapsed until baronial reassertion, it was constitutive.',
    'If inseparable, the 1216 reissue terminates the original constraint and later instruments are successors rather than continuations — collapsing this story''s effective interval at approximately t=2 and dating the mandate''s completion far earlier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_clause_separability, empirical, 'Whether the enforcement machinery is part of the constraint or an accessory to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_originalist_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(mc39_originalist_tr_t0, observed).
narrative_ontology:measurement(mc39_originalist_tr_t2, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement_basis(mc39_originalist_tr_t2, observed).
narrative_ontology:measurement(mc39_originalist_tr_t10, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(mc39_originalist_tr_t10, observed).
narrative_ontology:measurement(mc39_originalist_tr_t20, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(mc39_originalist_tr_t20, observed).
narrative_ontology:measurement(mc39_originalist_tr_t30, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(mc39_originalist_tr_t30, observed).
narrative_ontology:measurement(mc39_originalist_tr_t40, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(mc39_originalist_tr_t40, observed).
narrative_ontology:measurement(mc39_originalist_tr_t50, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(mc39_originalist_tr_t50, observed).
narrative_ontology:measurement(mc39_originalist_tr_t60, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(mc39_originalist_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(mc39_originalist_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(mc39_originalist_be_t0, observed).
narrative_ontology:measurement(mc39_originalist_be_t2, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(mc39_originalist_be_t2, observed).
narrative_ontology:measurement(mc39_originalist_be_t10, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(mc39_originalist_be_t10, observed).
narrative_ontology:measurement(mc39_originalist_be_t20, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(mc39_originalist_be_t20, observed).
narrative_ontology:measurement(mc39_originalist_be_t30, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement_basis(mc39_originalist_be_t30, observed).
narrative_ontology:measurement(mc39_originalist_be_t40, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(mc39_originalist_be_t40, observed).
narrative_ontology:measurement(mc39_originalist_be_t50, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(mc39_originalist_be_t50, observed).
narrative_ontology:measurement(mc39_originalist_be_t60, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(mc39_originalist_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc39_originalist_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(mc39_originalist_su_t0, observed).
narrative_ontology:measurement(mc39_originalist_su_t2, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 2, 0.66).
narrative_ontology:measurement_basis(mc39_originalist_su_t2, observed).
narrative_ontology:measurement(mc39_originalist_su_t10, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(mc39_originalist_su_t10, observed).
narrative_ontology:measurement(mc39_originalist_su_t20, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(mc39_originalist_su_t20, observed).
narrative_ontology:measurement(mc39_originalist_su_t30, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(mc39_originalist_su_t30, observed).
narrative_ontology:measurement(mc39_originalist_su_t40, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement_basis(mc39_originalist_su_t40, observed).
narrative_ontology:measurement(mc39_originalist_su_t50, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(mc39_originalist_su_t50, observed).
narrative_ontology:measurement(mc39_originalist_su_t60, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(mc39_originalist_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the eps-invariance principle: the colloquial label 'Magna Carta clause 39' covers three structurally distinct claims that must not share one story. This file (originalist_limitation_reading) authors the documented-grant baseline: bounded scope, 1215 victim set, moderate epsilon. The liberal_due_process sibling authors the universal-rights claim (expanded protected class, higher epsilon, centuries-long reach); the feudal_prerogative sibling authors the hierarchical-procedure claim (narrowest scope, procedural formality). This story is the evidentiary upstream: both siblings cite the clause's words, and the documented 1215 context is the record against which any reading's scope claim is checked — the liberal reading cites the text while discarding the baseline, the feudal reading retains the baseline's narrowness while discarding its grievance-specificity. Family membership is recorded via affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
