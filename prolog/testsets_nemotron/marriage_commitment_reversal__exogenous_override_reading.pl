% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercion of LDS Marriage Practice (1890 Manifesto — Exogenous Override Reading)
 *   domain: religious_institutional/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   marriage_commitment_reversal kernel: the 1890 Manifesto (Official
 *   Declaration 1) is authored as a capitulation to federal coercion —
 *   legislative disincorporation, asset seizure, imprisonment of leadership,
 *   and the Supreme Court's validation of anti-polygamy statutes — without
 *   internal doctrinal revision. The underlying revelation (D&C Section 132,
 *   1843) is preserved as canonical scripture and temple liturgy. The
 *   doctrine-practice gap is structural: the principle is eternal, the
 *   practice suspended. The federal government extracts institutional
 *   autonomy as the price of Utah statehood; the LDS Church pays with its
 *   distinctive marriage covenant. The constraint operates as a snare from
 *   the LDS institutional seat: high extraction (0.78 at peak), extreme
 *   suppression (0.91), active enforcement required, identifiable victims
 *   (rank-and-file in plural marriages), and no coordination benefit to the
 *   payer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.78).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.91).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercion of LDS Marriage Practice (1890 Manifesto — Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '109c6e88-a986-472e-a56d-ec9ed8c8010f').
narrative_ontology:cs_kernel_codification('109c6e88-a986-472e-a56d-ec9ed8c8010f', fixed_text).
narrative_ontology:cs_authority_grounding('109c6e88-a986-472e-a56d-ec9ed8c8010f', extraction).
narrative_ontology:cs_interpretation_layer_present('109c6e88-a986-472e-a56d-ec9ed8c8010f').
narrative_ontology:cs_reading_relation('109c6e88-a986-472e-a56d-ec9ed8c8010f', marriage_commitment_reversal__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('109c6e88-a986-472e-a56d-ec9ed8c8010f', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('109c6e88-a986-472e-a56d-ec9ed8c8010f', foundational, manifesto_issued_under_duress).
narrative_ontology:cs_axiom_status(manifesto_issued_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('109c6e88-a986-472e-a56d-ec9ed8c8010f', manifesto_issued_under_duress, empirically_contingent).
narrative_ontology:cs_axiom('109c6e88-a986-472e-a56d-ec9ed8c8010f', foundational, section_132_never_renounced).
narrative_ontology:cs_axiom_status(section_132_never_renounced, holdable).
narrative_ontology:cs_axiom_grounding('109c6e88-a986-472e-a56d-ec9ed8c8010f', section_132_never_renounced, conventional).
narrative_ontology:cs_reference_frame('109c6e88-a986-472e-a56d-ec9ed8c8010f', divine_mandate_plural_marriage_1843).
narrative_ontology:cs_drift_state('109c6e88-a986-472e-a56d-ec9ed8c8010f', manifesto_1890, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('109c6e88-a986-472e-a56d-ec9ed8c8010f', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_control).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, lds_leadership_quorum).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_rank_and_file).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_leadership_quorum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The U.S. federal government (Congress, Executive, and Supreme Court) wields legislative, judicial, and executive power to compel the LDS Church to abandon plural marriage as a condition of Utah statehood and institutional survival. It enacts the Edmunds Act (1882), Edmunds-Tucker Act (1887), and related statutes, disincorporates the Church, seizes its assets, and threatens imprisonment of leadership. The federal government extracts institutional autonomy as the price of territorial integration; it benefits by securing sovereign control over a contested western territory and establishing the precedent that religious practice yields to federal law. Exit is arbitrage-grade: the federal state faces no structural constraint on its coercive capacity in this domain.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_control, agenda_setter,
    institutional, generational, arbitrage, national).

% The LDS Church as a corporate and spiritual entity bears the full cost of the constraint: it surrenders its distinctive marriage practice, accepts federal receivership of its assets, submits to loyalty oaths for leadership, and endures the imprisonment of its officers. Its exit options are identity-locked — the Church's self-conception as a covenant people bound to restored ordinances makes doctrinal capitulation an existential fracture, yet resistance means institutional dissolution. The 1890 Manifesto (Official Declaration 1) is issued under explicit duress ('I saw exactly what would come to pass if there was not something done' — Woodruff) without renouncing the underlying revelation (Section 132). The doctrine-practice gap persists: the principle is preserved in scripture and temple liturgy while public practice is suspended.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty, payer,
    organized, biographical, identity_locked, national).

% Ordinary Latter-day Saints who entered plural marriages on religious conviction face criminal prosecution, disenfranchisement, property loss, and social stigma. They are neither consulted on the Manifesto nor represented in the negotiation; their marriages are retroactively delegitimized by the institution they sustained. Exit is constrained: geographic mobility is limited, religious identity is fused to the community, and the federal threat targets their material survival. They bear the lived cost of the doctrine-practice gap — taught that plural marriage is an eternal principle while watching the institution comply with its prohibition.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_rank_and_file, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_rank_and_file, excluded).

% The First Presidency and Quorum of the Twelve Apostles occupy a dual position: they authorize the Manifesto to preserve institutional continuity (beneficiary of survival) but bear the spiritual and institutional cost of submitting to external coercion without doctrinal revision (payer of autonomy). Their identity is locked to the prophetic office — resignation or schism would fracture the claimed line of authority. They manage the doctrine-practice gap by declaring the practice 'suspended' while preserving the principle, creating a structural ambiguity that persists for decades (post-Manifesto plural marriages continue covertly until the Second Manifesto, 1904).
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_leadership_quorum, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_leadership_quorum, payer).

% The U.S. Supreme Court (Davis v. Beason, 1890; Late Corp. of the Church v. United States, 1890) upholds the constitutionality of anti-polygamy statutes, ruling that religious belief is protected but religious practice can be regulated. The judiciary provides the legal architecture that makes the coercion legitimate and enforceable. It does not collect the extraction but validates the mechanism. Its seat is analytical: it observes the structural relationship and authorizes the state's coercive power.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Protestant missionary networks, women's organizations (e.g., Women's National Anti-Polygamy Society), and congressional allies who framed plural marriage as 'the twin relic of barbarism' alongside slavery. They lobbied for the legislation that enabled federal coercion. They are excluded from the LDS Church's internal decision-making but their political pressure is the proximal cause of the constraint's activation. Their exit is mobile — they operate in the national public sphere and can shift strategies. They would object to any reading that treats the Manifesto as a genuine internal revelation rather than a capitulation to their campaign.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_reformers, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal government coordinates territorial governance by requiring religious minorities to conform to a monogamous marital norm that underwrites property law, inheritance, and civic status across the Union. The constraint solves a genuine coordination problem: a unified legal framework for marriage across federal territories.
% TRANSFER_FUNCTION: Moves institutional autonomy, property, and religious liberty from the LDS Church to the federal state. The Church surrenders its distinctive practice and accepts federal supremacy; the federal state gains enforceable territorial control and a precedent limiting free-exercise claims.
% ABSENT_VOICES: LDS rank-and-file members in plural marriages — especially women — had no voice in the Manifesto's issuance. Their marriages were dissolved or driven underground without consultation. Anti-polygamy reformers, while politically potent, were excluded from the Church's internal deliberation and would reject any framing that treats the reversal as internally generated.
% DISAPPEARANCE_RATIONALE: If the federal coercion and the Manifesto vanished overnight, the LDS Church would face immediate re-litigation of its corporate status, property seizures, and the theological crisis of a living prophet having reversed a claimed eternal covenant under duress. The doctrine-practice gap would collapse into either open schism (fundamentalist continuation) or doctrinal repudiation (Section 132 renounced). The federal territorial settlement of the Intermountain West would be legally destabilized.
% FOUNDING_PROBLEM: The federal government needed to integrate Utah Territory into the Union under a uniform marital law; the LDS Church claimed a divine mandate for plural marriage that conflicted with federal sovereignty. The constraint was built to resolve this sovereignty collision by forcing the Church to yield practice while preserving its institutional existence.
% FOUNDING_PROBLEM_CORROBORATION: Utah statehood achieved 1896; federal anti-polygamy enforcement lapsed after 1904 (Second Manifesto). The sovereignty collision is historically resolved — the federal state prevails, the Church survives. However, the LDS Church's own official materials (Gospel Topics Essays, 2014) acknowledge the Manifesto was issued under 'intense pressure' and 'the Lord showed me exactly what would come to pass if we did not stop.' The doctrinal principle (Section 132) remains canonized. No external corroboration supports the claim that the founding problem (federal territorial integration) requires the constraint's continued operation; the Church's internal narrative treats the founding problem as resolved by divine accommodation, not federal victory.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at the Manifesto (1890) when the Church formally surrenders its core distinctive practice under explicit threat of institutional death. Suppression is highest then because the enforcement machinery (Edmunds-Tucker Act, disfranchisement, property seizure, loyalty oaths) is fully deployed. Theater ratio rises through the 1880s as the Church performs resistance (underground plural marriages, 'the Underground') while the federal noose tightens; the 1890 Manifesto itself is a performative act — Woodruff's language ('I saw exactly what would come to pass') signals coerced compliance, not revelation. Post-1890, extraction declines as Utah achieves statehood (1896) and the Second Manifesto (1904) ends new plural marriages, but the doctrine-practice gap persists as structural theater: the principle remains canonized, the practice remains prohibited.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is a rope: it coordinates a unified marital law across territories, solves a genuine collective-action problem (territorial governance), and participants (the nation) are net beneficiaries. From the LDS institutional seat, it is a snare: extraction is asymmetric, enforcement is coercive, alternatives (religious liberty, territorial self-governance) are suppressed, and victims are identifiable. The engine computes this divergence from the structural data — the claimed_type (snare) reflects the authoring seat's structural reading, not a compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal territorial control is the agenda-setter and beneficiary: it sets the terms, enforces them, and extracts institutional autonomy. LDS institutional sovereignty is the primary payer: identity-locked exit, organized power but no arbitrage, bears the full cost of capitulation. LDS rank-and-file are victims: constrained exit, moderate power, bear lived costs without voice. LDS leadership quorum is dual-positioned: beneficiary of institutional survival, payer of prophetic credibility, identity-locked in both directions. Federal judiciary is analytical observer. Anti-polygamy reformers are excluded mobilizers. Directionality derives from beneficiary/victim declarations + exit modulation: federal d ≈ 0.15 (beneficiary), LDS institution d ≈ 0.85 (target), rank-and-file d ≈ 0.9 (target, identity-locked), leadership d ≈ 0.7 (dual, identity-locked).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (federal territorial integration under uniform marital law) is dead — achieved 1896. Yet the constraint's legacy persists: the doctrine-practice gap (Section 132 canonized, plural marriage prohibited) remains a structural feature of LDS identity. The founding problem is dead but the arrangement's spectral trace operates as a piton-like residue: the Church maintains the principle while complying with the prohibition, creating a permanent ambiguity that fundamentalist schisms exploit. This is not mandatrophy in the simple sense (an agency outliving its function) but a kernel-level doctrinal fracture preserved by the exogenous override.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_capitulation_ambiguity,
    'Does the Manifesto''s language (''The Lord showed me exactly what would come to pass'') represent a genuine revelatory reinterpretation or a performative framing of capitulation?',
    'Contemporaneous private records (Woodruff''s journal, Quorum minutes, 1889-1890 correspondence) vs. public rhetoric. If private deliberation shows revelation preceding pressure, endogenous reading gains; if pressure precedes revelation language, exogenous reading gains.',
    'If genuine revelation, the constraint reclassifies toward tangled_rope (coordination + residual extraction). If performative capitulation, snare classification holds. The kernel''s classification hinges on this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_capitulation_ambiguity, conceptual, 'Whether the Manifesto''s internal framing matches its external causation.').

omega_variable(
    doctrine_practice_gap_persistence,
    'Why does the LDS Church preserve Section 132 as canon while prohibiting the practice it authorizes? Is this theological coherence or institutional management of the exogenous override''s legacy?',
    'Track official discourse (General Conference, Gospel Topics Essays, temple liturgy changes) over 1890-present. If the gap narrows (principle reinterpreted, not preserved), the exogenous override''s structural trace decays. If the gap widens (fundamentalist schisms, temple sealing theology), the trace persists.',
    'Persistent gap = the exogenous override created a permanent structural fracture in the kernel. Narrowing gap = the kernel is healing toward endogenous reinterpretation. The practice_doctrine_gap sibling reading is the live hypothesis for the persistent-gap trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_practice_gap_persistence, empirical, 'Whether the doctrine-practice gap is a stable structural feature or a transitional state.').

omega_variable(
    federal_extraction_scope,
    'Did federal coercion extract only plural marriage practice, or did it establish a broader precedent limiting religious institutional autonomy that shapes subsequent free-exercise jurisprudence?',
    'Trace Reynolds v. United States (1879), Davis v. Beason (1890), Late Corp. v. United States (1890) through Employment Division v. Smith (1990) and subsequent religious liberty cases. If the polygamy cases are cited as foundational for belief-action distinction, extraction scope is broader.',
    'Broader scope = federal beneficiary extracts not just LDS autonomy but a general constraint on religious institutional power. Narrower scope = extraction is specific to the territorial sovereignty collision. Affects network.affects_constraints linking to religious_liberty_precedent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_extraction_scope, empirical, 'Whether the extraction''s scope is specific to LDS plural marriage or general to religious institutional autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1882, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.15).
narrative_ontology:measurement(marr_tr_t1884, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1884, 0.22).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.31).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.38).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1896, 0.35).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1904, 0.32).

% Extraction over time
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.42).
narrative_ontology:measurement(marr_be_t1884, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1884, 0.55).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.71).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.78).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1896, 0.68).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1904, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.55).
narrative_ontology:measurement(marr_su_t1884, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1884, 0.68).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.82).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.91).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1896, 0.78).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1904, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, religious_liberty_precedent__belief_action_distinction).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, utah_statehood_compact).

% DUAL FORMULATION NOTE:
% This constraint family (marriage_commitment_reversal) decomposes the single historical event (1890 Manifesto) into three structurally distinct readings. The exogenous_override_reading (this story) has high extractiveness (0.78) and snare classification because the federal government extracts institutional autonomy without doctrinal revision. The endogenous_reinterpretation_reading would have lower extractiveness (~0.35) and tangled_rope classification because it posits genuine revelatory coordination alongside residual federal pressure. The practice_doctrine_gap is not a temporal stage but a structural ambiguity: the kernel (Section 132 preserved, practice suspended) instantiates a constraint whose type depends on which reading's causal structure you accept. All three stories share the same interval (1882-1904) and stakeholder set but differ in beneficiary/victim structure, claimed_type, and cs_structure axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, organized, 0.7).
constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
