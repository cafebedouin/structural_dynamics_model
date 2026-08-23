% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command — Continuationist Reading: Polygamy Doctrinally Valid, Manifesto Prudential Suspension
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The continuationist reading of the divine marriage command holds that the
 *   1843 revelation (D&C 132) establishing plural marriage as an eternal
 *   covenant remains doctrinally valid. The 1890 Manifesto and 1904 Second
 *   Manifesto are read as prudential suspensions under federal duress — not
 *   doctrinal rescissions. The institutional LDS Church maintains this
 *   framing officially while actively excommunicating practitioners.
 *   Fundamentalist groups (FLDS, AUB, independents) claim continuity with the
 *   original revelation and bear the full extraction of federal and
 *   institutional enforcement. The constraint is the standing arrangement:
 *   federal prohibition + institutional suspension + fundamentalist
 *   persistence. This reading sees the arrangement as a tangled rope —
 *   genuine coordination (church survival) paired with asymmetric extraction
 *   (fundamentalists pay the cost).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.72).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.78).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command — Continuationist Reading: Polygamy Doctrinally Valid, Manifesto Prudential Suspension").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '0c685680-966e-4378-b750-e9ba97d86c2f').
narrative_ontology:cs_kernel_codification('0c685680-966e-4378-b750-e9ba97d86c2f', formalized).
narrative_ontology:cs_authority_grounding('0c685680-966e-4378-b750-e9ba97d86c2f', lineage).
narrative_ontology:cs_interpretation_layer_present('0c685680-966e-4378-b750-e9ba97d86c2f').
narrative_ontology:cs_reading_relation('0c685680-966e-4378-b750-e9ba97d86c2f', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0c685680-966e-4378-b750-e9ba97d86c2f', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('0c685680-966e-4378-b750-e9ba97d86c2f', foundational, plural_marriage_eternal_command).
narrative_ontology:cs_axiom_status(plural_marriage_eternal_command, holdable).
narrative_ontology:cs_axiom_grounding('0c685680-966e-4378-b750-e9ba97d86c2f', plural_marriage_eternal_command, theological).
narrative_ontology:cs_axiom('0c685680-966e-4378-b750-e9ba97d86c2f', foundational, manifesto_prudential_not_doctrinal).
narrative_ontology:cs_axiom_status(manifesto_prudential_not_doctrinal, holdable).
narrative_ontology:cs_axiom_grounding('0c685680-966e-4378-b750-e9ba97d86c2f', manifesto_prudential_not_doctrinal, deontological).
narrative_ontology:cs_axiom('0c685680-966e-4378-b750-e9ba97d86c2f', secondary, fundamentalist_continuity_claim).
narrative_ontology:cs_axiom_status(fundamentalist_continuity_claim, holdable).
narrative_ontology:cs_axiom_grounding('0c685680-966e-4378-b750-e9ba97d86c2f', fundamentalist_continuity_claim, conventional).
narrative_ontology:cs_reference_frame('0c685680-966e-4378-b750-e9ba97d86c2f', eternal_plural_marriage_command).
narrative_ontology:cs_drift_state('0c685680-966e-4378-b750-e9ba97d86c2f', contemporary_institutional_accommodation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c685680-966e-4378-b750-e9ba97d86c2f', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, lds_institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, mainstream_lds_members).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, federal_state).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, fundamentalist_polygamists).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamous_families).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, plural_marriage_eternal_command).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, revelation_irreversibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto suspending polygamy under federal threat of disincorporation and asset seizure. Maintains the Manifesto was prudential, not doctrinal, but enforces excommunication for polygamy practice. Benefits from institutional survival, property retention, and mainstream legitimacy. Can navigate legal and political systems; exit options include doctrinal reinterpretation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, lds_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain institutional stability, social acceptance, and legal protection from the Manifesto's compliance. Most do not practice polygamy and view the suspension as settled. Exit is relatively easy — they remain in the institutional church or leave for other faiths. The constraint's coordination function (community cohesion) benefits them directly.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_lds_members, beneficiary,
    organized, biographical, mobile, global).

% Enacted and enforced anti-polygamy legislation (Edmunds Act, Edmunds-Tucker Act) threatening church disincorporation. Achieved de facto monogamy compliance from the institutional church. Extracts sovereignty over marriage law and territorial governance. Exit options are maximal — the state sets the legal framework.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, federal_state, beneficiary).

% Continue plural marriage as divine commandment, rejecting the Manifesto as binding doctrine. Face felony prosecution, property seizure, child custody loss, and social marginalization. Their identity is fused to the practice — exit requires abandoning core theological self-understanding. They view the institutional church as apostate.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_polygamists, payer,
    powerless, biographical, identity_locked, regional).

% Women and children in plural families bear disproportionate costs: legal vulnerability, economic precarity, isolation from mainstream services, psychological strain from secrecy. Exit is structurally blocked by economic dependency, religious socialization, and fear of state intervention. Their situation is not chosen but inherited.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Scholars, feminists, and progressive members within the institutional church who question polygamy's doctrinal status or the Manifesto's framing. Their voices are marginalized in official discourse; correlation committees and disciplinary councils limit institutional platform. Exit means leaving the church or silencing.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, dissident_lds_voices, excluded,
    moderate, biographical, constrained, national).

% Sees the full structure: a divine command claimed as eternal, suspended by institutional leadership under state coercion, maintained by fundamentalists at high cost, with the institutional church performing compliance while disavowing the practice. No material stake in the outcome.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the institutional church as a legal corporate entity capable of holding property, operating temples, and maintaining global missionary infrastructure — coordination that would have been destroyed by federal disincorporation.
% TRANSFER_FUNCTION: Moves the cost of federal compliance from the institutional leadership (who would lose corporate existence) onto fundamentalist polygamists (who bear prosecution, stigma, and family disruption) and polygamous families (who bear secrecy, legal vulnerability, and economic marginalization). The institutional church transfers extraction onto its most committed adherents.
% ABSENT_VOICES: Polygamous women and children — especially those in fundamentalist communities — are structurally excluded from doctrinal decision-making. Their experience of the constraint (coercion, isolation, legal terror) is not represented in the institutional church's 'prudential suspension' narrative or the fundamentalist 'faithful continuity' narrative. They are the silent core of the extraction.
% DISAPPEARANCE_RATIONALE: If the anti-polygamy enforcement and institutional suspension vanished overnight, the institutional church would face a schism crisis: fundamentalists would claim vindication and demand reinstatement; mainstream members would face doctrinal whiplash; the federal-state/church compact would dissolve. The arrangement currently holds a fragile equilibrium.
% FOUNDING_PROBLEM: The 1843 revelation (D&C 132) commanding plural marriage as an eternal covenant created a divine mandate that conflicted with U.S. federal law and territorial statehood aspirations. The founding problem: how to maintain the church as a viable institution under U.S. sovereignty without doctrinally rescinding a revelation claimed as eternal.
% FOUNDING_PROBLEM_CORROBORATION: The institutional church's own Gospel Topics essays acknowledge the Manifesto was issued under 'threat of destruction.' Fundamentalist groups (FLDS, AUB, etc.) attest the founding problem is live — the revelation remains binding. Independent historians (e.g., Kathleen Flake, Sarah Barringer Gordon) corroborate that federal coercion drove the Manifesto, not internal doctrinal development. No party outside the institutional leadership claims the founding problem is resolved.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers the cost of institutional survival onto the most doctrinally committed. Suppression (0.78) is high due to felony statutes, child protective interventions, and institutional excommunication. Theater ratio (0.45) reflects the institutional church's performance of compliance (excommunications, public statements) while the underlying doctrinal claim remains intact in fundamentalist practice and institutional eschatology. Accessibility collapse (0.62) is moderate — monogamy is legally accessible but doctrinally rejected by continuationists. Resistance (0.68) is high — fundamentalist communities persist despite 135 years of enforcement. The measurement grid shows extractiveness peaking at the 1890-1904 crisis, declining during mid-century raids, then rising slightly with recent legal challenges (Brown v. Buhman, Utah SB 102).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (institutional leadership, federal state) experience this as a solved coordination problem — the church survives, the law prevails. The payer seats (fundamentalists, families) experience it as ongoing extraction with no coordination benefit — they are coordinated OUT of the arrangement. The analytical observer sees the structural asymmetry: a constraint that claims divine origin but operates as a federal/church compact enforced on the powerless. The engine will compute per-seat types from this data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership and federal state are structural beneficiaries (d near 0.0-0.2) — the former retains corporate existence, the latter achieves legal monopoly over marriage. Mainstream members are near-symmetric (d ~0.45) — they gain stability but lose doctrinal coherence. Fundamentalist polygamists and their families are full targets (d ~0.9-1.0) — identity-locked and trapped respectively, bearing prosecution, stigma, and family disruption. The identity_locked status of fundamentalists is not metaphorical: their salvation narrative, family structure, and communal belonging are fused to plural marriage. Exit requires epistemic rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (church survival under U.S. sovereignty) was live in 1890. By 2025, the institutional church is a global corporation with $100B+ assets — the survival threat is gone. Yet the constraint persists. The continuationist reading diagnoses this as mandatrophy: the original mandate (survival) is achieved, but the suspension remains because doctrinal rescission would fracture the church's truth claims. The fundamentalist persistence is not a bug but a feature — their existence lets the institutional church claim the revelation was never rescinded, only suspended. The constraint has become a piton for the institution (theatrical maintenance of 'prudential suspension') but remains a snare for fundamentalists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the continuationist reading a genuine theological position or a post-hoc rationalization for fundamentalist persistence?',
    'Trace the genealogical emergence of ''prudential suspension'' language in fundamentalist discourse vs. institutional discourse. If the framing originates with fundamentalists post-1904, it may be a cover story; if it originates in 1890 leadership discourse, it has stronger claim to authenticity.',
    'If post-hoc rationalization, the continuationist reading''s claimed_type (tangled_rope) may mask a snare — the coordination function (church survival) would be a cover for fundamentalist extraction. If authentic, the tangled_rope classification stands: genuine coordination with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the continuationist framing is theologically authentic or a fundamentalist cover story').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of fundamentalist polygamists primarily structural (legal penalties) or internalized (theological identity fusion making exit unthinkable)?',
    'Post-exit trajectory study: track fundamentalists who leave plural marriage — if suppression persists (shame, ostracism, identity crisis) after legal threat removal, internalized component is significant.',
    'If substantially internalized, the constraint''s effective suppression is higher than legal measures suggest — the target carries the suppression post-exit. This would increase the measured suppression for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in fundamentalist polygamist communities').

omega_variable(
    coordination_extraction_boundary,
    'Is the institutional church''s survival (coordination function) genuinely dependent on the Manifesto''s suspension, or could the church have survived via alternative accommodation (e.g., territorial autonomy, constitutional challenge)?',
    'Counterfactual historical analysis: compare Utah statehood timeline with alternative legal strategies (Reynolds v. US dissent, Edmunds-Tucker Act political opposition). Assess whether disincorporation was truly existential or strategically chosen.',
    'If survival was achievable without doctrinal suspension, the coordination function is overstated — the constraint is more snare than tangled rope. If disincorporation was genuinely existential, the coordination function is real and the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination function (institutional survival) was genuinely necessary or strategically constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmc_cont_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(dmc_cont_tr_t1904, divine_marriage_command__continuationist_reading, theater_ratio, 1904, 0.35).
narrative_ontology:measurement(dmc_cont_tr_t1953, divine_marriage_command__continuationist_reading, theater_ratio, 1953, 0.42).
narrative_ontology:measurement(dmc_cont_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.48).
narrative_ontology:measurement(dmc_cont_tr_t2000, divine_marriage_command__continuationist_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(dmc_cont_tr_t2025, divine_marriage_command__continuationist_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(dmc_cont_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(dmc_cont_be_t1904, divine_marriage_command__continuationist_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement(dmc_cont_be_t1953, divine_marriage_command__continuationist_reading, base_extractiveness, 1953, 0.72).
narrative_ontology:measurement(dmc_cont_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(dmc_cont_be_t2000, divine_marriage_command__continuationist_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(dmc_cont_be_t2025, divine_marriage_command__continuationist_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dmc_cont_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(dmc_cont_su_t1904, divine_marriage_command__continuationist_reading, suppression_requirement, 1904, 0.85).
narrative_ontology:measurement(dmc_cont_su_t1953, divine_marriage_command__continuationist_reading, suppression_requirement, 1953, 0.8).
narrative_ontology:measurement(dmc_cont_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(dmc_cont_su_t2000, divine_marriage_command__continuationist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(dmc_cont_su_t2025, divine_marriage_command__continuationist_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.08).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint family (divine_marriage_command) decomposes the single label 'Mormon polygamy doctrine' into three structurally distinct constraints with different ε values and stakeholder structures. The continuationist_reading (this story) has high ε (0.72) because it centers fundamentalist extraction. The substitutionist_reading has low ε (~0.15) because it centers institutional resolution. The coercion_visibility_reading has moderate ε (~0.45) because it centers the coercion/survival tradeoff. They are linked via affects_constraints because the institutional church cites the Manifesto as evidence for all three readings depending on audience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, institutional, 0.15).
constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
