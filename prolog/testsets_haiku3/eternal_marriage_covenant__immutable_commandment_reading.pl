% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant as Immutable Divine Commandment
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   D&C 132 (Doctrine and Covenants section 132) is Joseph Smith's 1843
 *   revelation establishing plural marriage as eternally required for the
 *   highest form of exaltation in the celestial kingdom. This constraint
 *   story instantiates ONE READING of the contested kernel 'eternal marriage
 *   covenant' — specifically, the immutable_commandment_reading. This reading
 *   asserts that D&C 132 established plural marriage as an immutable,
 *   unchangeable divine law that cannot be suspended, revised, or overridden
 *   by any human authority, including later prophets. Under this reading,
 *   federal prosecution of polygamy creates an irresolvable collision:
 *   members are commanded by God to practice plural marriage and commanded by
 *   the state to cease. The reading forecloses any legitimate escape path for
 *   practitioners — compliance with the doctrine means legal liability;
 *   compliance with federal law means apostasy (rejecting an immutable
 *   commandment). The institutional church, under this reading, maintains the
 *   doctrine's immutability while suspending its practice via the 1890
 *   Manifesto, a logically incoherent position that the constraint analysis
 *   exposes. This story does NOT address the prophetic_override_reading
 *   (which asserts continuing revelation can revise D&C 132) or the
 *   temporal_accommodation_reading (which asserts the Manifesto suspended
 *   practice without renouncing the doctrine, legitimizing obedience to
 *   federal law). Those are separate constraints instantiating competing
 *   readings of the same kernel.
 *
 * KEY AGENTS:
 *   - Institutional Church Authority — the agenda-setter that interprets and enforces D&C 132 as immutable, gatekeeping temple access (the ordinance for exaltation) to ensure compliance
 *   - Plural Marriage Practitioners — the payers who obey the immutable commandment, bearing legal prosecution, social stigma, and family fragmentation; identity-locked to the faith (exit costs apostasy)
 *   - Federal Government — the counter-agenda-setter criminalizing polygamy and pressuring the church through prosecution; enforces the nation's sovereign authority over marriage law
 *   - Dissenting Theologians — the constrained payers advocating alternative readings (prophetic revision, temporal accommodation); risk excommunication if they publicize these readings inside the faith
 *   - Apostate Former Members — excluded from internal theological deliberation but would testify that immutability is a totalistic commitment device, not a genuine doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.81).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.88).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant as Immutable Divine Commandment").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '625075ed-0a42-4fb6-8be5-aae29f4cf392').
narrative_ontology:cs_kernel_codification('625075ed-0a42-4fb6-8be5-aae29f4cf392', fixed_text).
narrative_ontology:cs_authority_grounding('625075ed-0a42-4fb6-8be5-aae29f4cf392', lineage).
narrative_ontology:cs_interpretation_layer_present('625075ed-0a42-4fb6-8be5-aae29f4cf392').
narrative_ontology:cs_reading_relation('625075ed-0a42-4fb6-8be5-aae29f4cf392', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('625075ed-0a42-4fb6-8be5-aae29f4cf392', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('625075ed-0a42-4fb6-8be5-aae29f4cf392', foundational, plural_marriage_eternally_immutable).
narrative_ontology:cs_axiom_status(plural_marriage_eternally_immutable, holdable).
narrative_ontology:cs_axiom_grounding('625075ed-0a42-4fb6-8be5-aae29f4cf392', plural_marriage_eternally_immutable, deontological).
narrative_ontology:cs_axiom('625075ed-0a42-4fb6-8be5-aae29f4cf392', secondary, immutability_precludes_prophetic_override).
narrative_ontology:cs_axiom_status(immutability_precludes_prophetic_override, holdable).
narrative_ontology:cs_axiom_grounding('625075ed-0a42-4fb6-8be5-aae29f4cf392', immutability_precludes_prophetic_override, deontological).
narrative_ontology:cs_reference_frame('625075ed-0a42-4fb6-8be5-aae29f4cf392', d_c_132_immutable_eternal_order).
narrative_ontology:cs_drift_state('625075ed-0a42-4fb6-8be5-aae29f4cf392', contemporary_post_manifesto_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('625075ed-0a42-4fb6-8be5-aae29f4cf392', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, institutional_church_authority).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_marriage_practitioners).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, monogamous_converts).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, monogamous_converts).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces D&C 132 as the authoritative reading of eternal marriage. Claims sole authority to adjudicate what the immutable commandment requires and permits. Collects the moral authority and institutional loyalty that flows from members' compliance with the doctrine. Controls admission to temple ordinances (the highest ritual rank in the religion) — compliance with this doctrine is gatekeeping for salvation in the framework.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, institutional_church_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Obey the immutable commandment by entering plural marriages and raising families in that form. Bear legal persecution (the federal government criminalizes polygamy), social stigma, family fragmentation when arrest or conversion separates spouses, and resource strain (plural families are economically precarious). Cannot exit without renouncing the core salvific framework (if you stop believing plural marriage is eternal, you forfeit exaltation in the cosmology). The reading offers them no legitimate exit path: the commandment is immutable; federal law is hostile; the church offers no revelation-based release.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_marriage_practitioners, payer,
    powerless, biographical, identity_locked, continental).

% Convert to the faith and are taught that monogamy is not the highest form of marriage — it is temporary, for this life only, not eternal. If they enter into polygamy per the doctrine, they suffer the same legal and social costs as born members. If they remain monogamous, they accept lower salvific status within the framework. Their choice set is bounded: accept polygamy with its legal consequences, accept diminished eternal standing, or leave the faith entirely.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, monogamous_converts, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, monogamous_converts, beneficiary).

% Criminalizes polygamy as bigamy and adultery, prosecutes practitioners, pressures the church through legal action and territorial governance. Bears the cost of enforcement (prosecution, imprisonment). The federal position is hostile to the theological claim that the commandment is immutable — federal law asserts the nation's sovereign authority over marriage law and can override any religious doctrine that contradicts it.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, federal_government, payer).

% Interpret D&C 132 differently — either as subject to prophetic revision (the prophetic_override_reading) or as suspended by the Manifesto (the temporal_accommodation_reading). Advocating these readings within the institutional church risks excommunication, loss of community, loss of temple access, and forced choice between doctrinal conscience and institutional belonging. Outside the church, their theological work has little institutional platform.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, regional).

% Analyze the collision between religious liberty (the church's claim to immutable doctrine) and secular law (the state's prohibition of polygamy). They document the constraint's dynamics but have no enforcement power. Their testimony and analysis inform public debate and legal proceedings.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, legal_pluralists_scholars, observer,
    organized, generational, mobile, national).

% Left the faith, often after realizing the immutability claim forecloses legitimate exit from the doctrine. They would testify that the constraint operates as a totalistic commitment device — you cannot stay in the faith and reject the doctrine, and you cannot leave without apostasy. They are structurally excluded from the church's internal conversation about the doctrine's interpretation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, apostate_former_members, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, institutional_church_authority).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: D&C 132, under this reading, coordinates the celestial kingdom hierarchy: plural marriage is presented as the mechanism for maximal exaltation — one man, many women, endless progeny in the afterlife. It solves the theological problem of how inequality and hierarchy in the cosmos are justified (those with more wives have higher status eternally). The coordination function is entirely internal to the religious cosmology — it has no secular analogue.
% TRANSFER_FUNCTION: Transfers moral authority and institutional loyalty from members to the church hierarchy. Members surrender individual judgment about the immutability of doctrine and obey institutional interpretation. The church consolidates power through gatekeeping temple access (the ordinances that confer eternal status are administered only by the institutional authority). Transfers legal and social burden to practitioners (legal prosecution, social stigma, family instability).
% ABSENT_VOICES: Practitioners who left the faith and those imprisoned or prosecuted for polygamy are structurally outside the church's internal adjudication process. They would testify that the immutability claim is a cover for institutional power consolidation — that the doctrine serves the church's authority interests, not the practitioners' salvific interests. The federal government's voice is explicitly excluded from the theological conversation (the reading asserts the commandment is beyond federal law's reach).
% DISAPPEARANCE_RATIONALE: From the institutional reading, if the immutable commandment disappeared, salvation as the church teaches it would become impossible — the celestial hierarchy would collapse, exaltation would lose its mechanism. From the dissenting readings and secular analysis, if the immutable claim disappeared and the church accepted prophetic revision or temporal accommodation, members would reorganize around monogamy and the faith would normalize to mainstream Protestantism. The two seats predict opposite rearrangements because they dispute what the doctrine is actually FOR.
% FOUNDING_PROBLEM: The founding problem (per Joseph Smith's account, 1831–1844) was doctrinal: God requires plural marriage as the eternal form of marriage and the mechanism for maximal exaltation in the afterlife. This is asserted as a revealed principle, not negotiated, and not subject to rational objection or external law.
% FOUNDING_PROBLEM_CORROBORATION: The institutional church attests that the founding problem is eternally live — plural marriage remains the highest form of marriage in the faith's cosmology. Dissenting theologians and apostate members attest that the founding problem is dead — the practice was abandoned, the doctrine is theologically indefensible to modern members, and the church's claim of immutability is a historical artifact maintained for institutional authority, not salvific function. Legal scholars document that the 1890 Manifesto (which suspended the practice) treated the problem as solved by obedience to federal law, implying the problem was not immutable after all — corroboration from secular sources that the institutional immutability claim is incoherent with the church's own past actions.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, contested).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as snare because the structure exhibits pure extraction under the cover of immutable doctrine. Extractiveness is high (0.81) because the arrangement persists by coercing compliance with a doctrine that serves institutional authority interests (gatekeeping temple access, consolidating member loyalty) more than practitioners' salvific interests. The practitioners have no legitimate exit — they cannot stay in the faith and reject the doctrine (immutability forecloses that), and they cannot leave without apostasy. Suppression is high (0.88) because the constraint is maintained by multiple layers: federal legal coercion (polygamy is criminalized), institutional religious coercion (temple access is conditional), and internalized identity fusion (members have fused their sense of self with the faith and the doctrine — leaving is ego-death). Theater_ratio is elevated (0.62) because the institutional church's public narrative emphasizes the doctrine's salvific necessity (celestial kingdom exaltation), while the actual enforcement function is gatekeeping institutional loyalty and maintaining authority over members. The measurement trajectory shows extraction and theater rising over the interval while suppression plateaus — as federal pressure eased (Utah achieved statehood in 1896), the church's need for active suppression machinery eased, but the doctrine's institutional function in consolidating authority intensified, and the theatrical justification (why members must continue believing in immutable plural marriage even though it is not practiced) became more elaborate. Accessibility collapse is high (0.79) because once members accept the immutability claim, alternative interpretations (prophetic override, temporal accommodation) collapse as psychologically illegible — they seem like apostasy, not theology. Resistance is high (0.72) because dissenting theologians and apostate former members persistently challenge the immutability claim with legal and textual arguments, and federal law enforcement provided structural resistance during the 19th century.
 *
 * PERSPECTIVAL GAP:
 *   The institutional agenda-setter and the identity-locked practitioners should compute different type classifications from the same structural data. From the institutional seat, the constraint is genuine coordination (the celestial kingdom doctrine requires plural marriage structure for exaltation; the church administers temple ordinances that make exaltation possible). From the practitioner seat, the constraint is pure extraction (they are commanded to practice something illegal, with no legitimate exit, no revision path, and no genuine salvific necessity — the doctrine serves the church's power consolidation, not their salvation). The engine's per-seat classification should flag this divergence: the institutional seat may compute rope or tangled_rope, while the practitioner seat computes snare. The claimed_type (snare) reflects the practitioner/dissenting seat reading; the institutional seat's reading would be rope or scaffolding-in-transition. This divergence IS the diagnostic content.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional church authority is the beneficiary (d ~ 0.0): the constraint consolidates their power, gates temple access, and ensures member loyalty through the immutability claim. Plural marriage practitioners are the targets (d ~ 1.0): they bear the costs of legal persecution, social stigma, family fragmentation, and totalistic commitment (identity-locked). Monogamous converts and dissenting theologians are constrained payers (d ~ 0.7–0.8): they face a choice set bounded by the doctrine's immutability — accept polygamy or diminished status. The federal government is a complex cross-seat: as an institutional agenda-setter it is opposed to the church (d ~ 0.5 from the church's view), but from the practitioner's view the federal legal coercion is an external constraint they also pay. The dissenting theologians are partially beneficiaries of federal law (it pressures the church toward revision) but payers in the faith (they risk excommunication for heresy). These relationships do not derive cleanly from a single power atom — the directionality override for dissenting_theologians adjusts d upward (toward target) to reflect their constrained position within the institutional faith, despite their moderate power in secular academic settings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem / founding_problem_status mismatch is the mandatrophy signature. The doctrine was founded to solve a theological problem: 'God requires plural marriage as the mechanism for maximal exaltation.' The founding_problem_status is contested — the institutional church claims the problem is eternally live, while dissenting theologians and apostate members claim it is dead (the celestial hierarchy doctrine does not require plural marriage; the practice was abandoned in 1890; modern members do not actually live it). If the founding problem is dead (the most coherent reading given the 1890 Manifesto), then the constraint is a zombie: it persists not because it solves the founding problem, but because institutional authority has an interest in maintaining it (gatekeeping, loyalty consolidation, theological coherence-by-fiat). The disappearance_verdict is contested for the same reason: from the institutional seat, if plural marriage disappeared entirely, the celestial hierarchy would collapse and exaltation would become impossible (the doctrine would be falsified). From the dissenting seat, if the doctrine disappeared, members would reorganize around monogamous exaltation and the faith would normalize (the problem it was founded to solve is already cosmologically solved — the Manifesto proved it). This reading claims mandatrophy is NOT resolved: the constraint persists in a dead-problem state, maintained by institutional authority theater and identity-locking suppression, not by genuine doctrinal necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_abandonment,
    'How can a doctrine be simultaneously immutable divine law and yet suspended by the 1890 Manifesto without being renounced? Does suspension preserve immutability or negate it?',
    'Textual analysis of church institutional statements: if the church claims the Manifesto suspended practice but preserved the doctrine, that is the assertion this reading evaluates. If the doctrine is truly immutable (unchangeable by any human authority), suspension without renunciation is logically incoherent — one or the other must be sacrificed (the doctrine is not immutable, or suspension is a temporary accommodation, not permanent policy).',
    'If the logical tension is unresolvable, the immutability claim is a false summit maintained for institutional authority rather than doctrinal coherence. The constraint would be reclassified as tangled_rope at best (coordination via celestial kingdom doctrine + asymmetric extraction via authority gatekeeping) or snare (pure extraction under cover of immutable doctrine). This omega directly affects the claimed_type verification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutability_vs_abandonment, conceptual, 'The logical incoherence between immutability and suspension contradictions.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'For identity-locked practitioners, is the measured suppression primarily structural (federal law makes exit from the practice costly but not from the faith; the faith itself offers no alternative path) or primarily internalized (members have accepted the doctrine as binding on their identity such that rejection feels like self-annihilation)?',
    'Post-exit suppression trajectory: members who leave the faith and the doctrine — do they report that the suppression (internal identity fusion, belief in immutability, shame at apostasy) persists after leaving, or does it dissolve once the structural constraints are removed? Longitudinal testimony from apostates on the mechanism of their exit.',
    'If suppression is heavily internalized, the constraint''s actual grip is higher than the structural measure suggests — members carry the suppression with them even after institutional exit. If structural, the constraint is contingent on the faith''s institutional authority and would degrade if the authority weakened. This affects the persistence prediction (how stable is the constraint if the church''s authority erodes?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is internalized or structural in identity-locked practitioners.').

omega_variable(
    prophetic_override_logical_status,
    'Within the faith''s own theological framework, is prophetic revision a coherent alternative to immutable commandment, or does immutability logically foreclose it?',
    'Detailed theological analysis: if a commandment is immutable (unchangeable), can a later prophet change it? The logical answer is no — immutability means no authority can alter it. If prophetic override is coherent, then either (a) immutability is not absolute, or (b) there are two classes of divine law (some immutable, some revisable), and plural marriage is being claimed as the first class. Church doctrine on which revelations are immutable vs. revisable.',
    'If prophetic override forecloses immutability logically, this reading (immutable_commandment_reading) and the prophetic_override_reading cannot coexist within a single coherent framework — they are forecloses, not coexists_with. If prophetic override is a coherent alternative doctrine, the two readings coexist across different theological factions. This is the crux of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prophetic_override_logical_status, conceptual, 'Whether immutability and prophetic revision are logically compatible within the faith''s own framework.').

omega_variable(
    kernel_reading_alternative_framings,
    'Is D&C 132 best understood as a commitment to an immutable doctrine, or as a commitment to following prophetic leadership (which then reinterpreted D&C 132 via the Manifesto)? Does the reading choice depend on what you take the core kernel to be?',
    'Textual analysis of Joseph Smith''s framing (1831–1844) vs. Brigham Young''s (1852–1877) vs. post-Manifesto institutional framing (1890–present). If the kernel is ''God revealed plural marriage,'' then the immutable reading asks ''Is that revelation immutable?'' If the kernel is ''Follow the prophet''s current direction,'' then plural marriage becomes a proxy for whatever the prophet currently enjoins — and the Manifesto becomes the authoritative update. The reading choice is not forced by the text; it is a framing choice about which commitment is primary.',
    'This reading instantiates one framing (immutable revelation is primary). The temporal_accommodation_reading and prophetic_override_reading instantiate different framings (prophetic authority is primary). The three readings may not be resolving a factual dispute so much as asserting different interpretive priorities. This affects how divergence is explained: not as competing empirical claims about what the doctrine is, but as competing commitments about which doctrine-about-doctrine is authoritative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Framing choice: immutable revelation vs. prophetic authority as primary commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(eter_tr_t15, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 15, 0.59).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(eter_be_t15, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(eter_su_t15, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. All three readings share the same doctrinal text (D&C 132) but instantiate different constraint structures depending on whether immutability is asserted (immutable_commandment_reading, this story), prophetic revision is authorized (prophetic_override_reading), or suspension is legitimized (temporal_accommodation_reading). The three readings have different ε values, different beneficiary/victim structures, and different computed types. They are linked by network.affects_constraints because the institutional authority's interpretation of any one reading cascades to affect the epistemic standing of the others — if the church declares immutability, it forecloses prophetic override; if it authorizes prophetic override, it retroactively falsifies immutability claims. The three stories decompose the kernel contest into structurally distinct constraints per OQ-26 (ε-invariance): each reading gets its own constraint_id, its own metrics, its own stakeholder analysis. This is not observer-dependent classification of a single constraint; it is a genuine decomposition of three different constraints that share a textual origin but have different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
