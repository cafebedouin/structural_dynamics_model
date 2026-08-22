% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Reading: Marriage Authority Belongs to Democratic Legislature; Personal Law Pluralism Is Transitional Anomaly Awaiting Elimination via Uniform Civil Code
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   The secularist reading of marriage authority asserts that legislative
 *   sovereignty over family law is a non-negotiable attribute of a modern
 *   democratic state. Personal law pluralism — the colonial-era arrangement
 *   allowing religious communities to govern marriage, divorce, and
 *   inheritance by their own norms — is characterized as a transitional
 *   anomaly that the Constitution's directive principles (Article 44) mandate
 *   eliminating through a Uniform Civil Code. This reading frames the UCC not
 *   as majoritarian imposition but as the completion of the constitutional
 *   project: a single legal order in which citizenship, not community
 *   membership, determines rights and obligations in the family. The
 *   constraint operates as a tangled rope because it simultaneously
 *   coordinates (provides a unified framework for gender equality,
 *   inter-community marriage, and legal certainty) and extracts (dismantles
 *   minority communities' jurisdictional autonomy, invalidates their
 *   interpretive traditions, and centralizes normative authority in a
 *   majoritarian legislature). The extraction is high and rising: what began
 *   as a coordination aspiration (1950s constitutional vision) has become an
 *   active enforcement project requiring constitutional amendment, judicial
 *   pressure, and political mobilization against communities that treat
 *   personal law as existential.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.78).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.72).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Reading: Marriage Authority Belongs to Democratic Legislature; Personal Law Pluralism Is Transitional Anomaly Awaiting Elimination via Uniform Civil Code").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '94881d69-55a1-4090-931c-08a5e744da1e').
narrative_ontology:cs_kernel_codification('94881d69-55a1-4090-931c-08a5e744da1e', formalized).
narrative_ontology:cs_authority_grounding('94881d69-55a1-4090-931c-08a5e744da1e', extraction).
narrative_ontology:cs_interpretation_layer_present('94881d69-55a1-4090-931c-08a5e744da1e').
narrative_ontology:cs_reading_relation('94881d69-55a1-4090-931c-08a5e744da1e', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('94881d69-55a1-4090-931c-08a5e744da1e', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('94881d69-55a1-4090-931c-08a5e744da1e', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('94881d69-55a1-4090-931c-08a5e744da1e', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('94881d69-55a1-4090-931c-08a5e744da1e', foundational, legislative_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('94881d69-55a1-4090-931c-08a5e744da1e', legislative_supremacy_in_family_law, conventional).
narrative_ontology:cs_axiom('94881d69-55a1-4090-931c-08a5e744da1e', foundational, personal_law_pluralism_as_colonial_anomaly).
narrative_ontology:cs_axiom_status(personal_law_pluralism_as_colonial_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('94881d69-55a1-4090-931c-08a5e744da1e', personal_law_pluralism_as_colonial_anomaly, conventional).
narrative_ontology:cs_axiom('94881d69-55a1-4090-931c-08a5e744da1e', secondary, uniform_civil_code_as_constitutional_completion).
narrative_ontology:cs_axiom_status(uniform_civil_code_as_constitutional_completion, holdable).
narrative_ontology:cs_axiom_grounding('94881d69-55a1-4090-931c-08a5e744da1e', uniform_civil_code_as_constitutional_completion, instrumental).
narrative_ontology:cs_reference_frame('94881d69-55a1-4090-931c-08a5e744da1e', constituent_assembly_directive_principle).
narrative_ontology:cs_drift_state('94881d69-55a1-4090-931c-08a5e744da1e', contemporary_hindutva_majoritarianism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('94881d69-55a1-4090-931c-08a5e744da1e', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, state_legislature).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, uniform_civil_code_advocates).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, personal_law_practitioners).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, communal_institutions).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, legislative_supremacy_in_family_law).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, constitutional_uniformity_over_pluralism).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, secularism_as_uniform_legal_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cross-party coalition of political parties, intellectuals, and civil society organizations that champions the UCC as the completion of India's secular constitutional project. They capture the normative capital of 'modernization' and 'gender justice,' control the legislative agenda through parliamentary majorities, and use the UCC as a mobilizational symbol. Their exit is arbitrage-grade: they can shift to other equality frameworks (judicial harmonization, community reform) without losing their core constituency or ideological coherence.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, arbitrage, national).

% The central legislative authority (Parliament) that would enact the UCC. It sets the legislative timetable, controls the drafting process, and bears the political costs of enactment. It benefits from the centralized normative authority the UCC confers — exclusive jurisdiction over family law eliminates the concurrent-list ambiguity and the need to negotiate with community bodies. Its exit is mobile: it could abandon the UCC project (as successive governments have) without institutional collapse, but the constraint's logic pulls toward enactment as legislative completion.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, state_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Lawyers, academics, women's rights activists, and litigants who strategically pursue UCC enactment or judicial approximation. They benefit professionally and ideologically from the constraint's momentum — UCC advocacy structures funding, career paths, and public recognition. Their exit is constrained: abandoning the UCC frame risks losing the strategic coherence of their advocacy, but they can pivot to judicial harmonization or community-based reform without total loss.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, uniform_civil_code_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Muslim, Christian, Parsi, and tribal communities for whom personal law is constitutive of communal identity, not merely a regulatory regime. The UCC threatens to extinguish their jurisdiction over marriage, divorce, inheritance, and adoption — domains that structure kinship, property transmission, and communal continuity. Their exit is identity_locked: leaving personal law means leaving the communal self; conversion, assimilation, or secularization are existential transformations, not policy choices. They bear the full extraction (loss of law-making authority) and the full suppression (judicial override, legislative threat, majoritarian rhetoric).
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, identity_locked, national).

% Qazis, church tribunals, Parsi anjuman trustees, customary law advisors — the interpretive and adjudicative infrastructure of personal law systems. Their professional authority, institutional relevance, and communal trust depend on the pluralist order. The UCC would render their expertise obsolete or subordinate to a unified statutory code. Exit is constrained: they can retrain in statutory family law, but the communal legitimacy and interpretive tradition they embody cannot be transferred.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, personal_law_practitioners, payer,
    moderate, biographical, constrained, national).

% All India Muslim Personal Law Board, Catholic Bishops' Conference, Parsi Anjuman, tribal customary councils — the corporate bodies that author, interpret, and administer personal law. They bear the extraction directly: the UCC eliminates their law-making function, transferring it to the legislature. Their suppression is active: they face judicial challenges to their adjudicative authority, legislative campaigns to abrogate their jurisdiction, and political rhetoric framing them as 'obstacles to national integration.' Exit is identity_locked: these institutions exist only through the personal law system; their dissolution or subordination is the constraint's intended outcome.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, communal_institutions, payer,
    organized, generational, identity_locked, national).

% Women within minority communities who experience gender inequality under personal law but reject the UCC as majoritarian imposition. They are structurally excluded from the secularist reading's beneficiary frame (which claims to speak for them) and from the communal_autonomy reading's authority frame (which treats community leadership as representative). Their exit is trapped: they cannot access UCC protections without the UCC's enactment (which threatens their communal belonging), and they cannot reform personal law from within without confronting patriarchal communal leadership. They would object to both the extraction (if UCC passes) and the status quo (if it doesn't), but neither reading's seat accommodates their position.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, women_in_minority_communities, excluded,
    powerless, biographical, trapped, national).

% The judicial seat that has incrementally imposed a constitutional floor on personal law (Shah Bano, Shayara Bano, Sabarimala) while avoiding a formal UCC mandate. It observes the constraint's operation from the analytical position — neither collecting the extraction nor bearing it — but its jurisprudence shapes the constraint's enforcement trajectory. The Court's analytical exit means it can shift between harmonization, deferral, and mandate without being trapped by any single reading.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% The statutory law-reform body that has produced multiple UCC consultation papers and draft reports. It occupies an analytical observer seat: it studies the constraint, recommends pathways, but does not enact or bear the extraction. Its reports are cited by all readings as evidence, but its institutional role is advisory, not decisive.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, law_commission, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, state_legislature).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified family law framework across all citizens: eliminates legal fragmentation that complicates inter-community marriage, inheritance, and child custody; establishes a single constitutional floor for gender equality in family relations; creates legal certainty for courts, administrators, and citizens navigating family disputes.
% TRANSFER_FUNCTION: Transfers legislative authority over marriage, divorce, inheritance, and adoption from community-based normative systems (personal law boards, religious tribunals, customary councils) to the central democratic legislature. The transfer moves normative authorship from communal interpretive traditions to majoritarian statutory enactment, and moves adjudicative authority from community tribunals to state courts applying a uniform code.
% ABSENT_VOICES: Women in minority communities who seek gender equality within their communal frameworks (excluded stakeholders) — they are claimed by the secularist reading as beneficiaries but are not consulted in the UCC's design. Tribal and indigenous communities with customary family laws not captured by the 'personal law' category — they would be absorbed by a UCC without representation. Federalist scholars who argue pluralism is a constitutional feature, not a bug — their structural argument is excluded from the majoritarian legislative frame.
% DISAPPEARANCE_RATIONALE: If the UCC constraint vanished overnight, the pluralist personal law order would persist: minority communities would retain jurisdiction, communal institutions would continue adjudicating, and the legislative agenda would shift from 'enacting UCC' to 'managing pluralism.' The secular_modernist_coalition would lose its defining mobilization target; the state_legislature would retain concurrent-list ambiguity; minority communities would avoid the existential threat of jurisdictional extinction. The world rearranges because the constraint's extraction is the active force preventing the pluralist equilibrium from stabilizing — its disappearance removes the suppression that keeps the transition 'transitional.'
% FOUNDING_PROBLEM: At Independence, India inherited a colonial personal law system that fragmented family law along religious lines, entrenched gender inequality within each community's code, and created legal uncertainty for inter-community families. The Constitution's framers included Article 44 (Uniform Civil Code) as a directive principle to resolve this fragmentation through a single secular family law, treating pluralism as a transitional colonial legacy rather than a constitutional commitment.
% FOUNDING_PROBLEM_CORROBORATION: The secularist coalition attests the founding problem is live: fragmentation persists, gender inequality persists, and the constitutional directive remains unfulfilled. Minority communities and federalist scholars attest the founding problem is substantially transformed: judicial harmonization has imposed constitutional floors on personal law, community-led reform has achieved significant gender-equality gains, and the pluralist order has stabilized as a consociational feature rather than a colonial anomaly. Independent legal historians (e.g., Granville Austin, Rohit De) and the Law Commission's own consultation reports corroborate that the 'transitional anomaly' framing was always contested — the Constituent Assembly debates record sharp disagreement on whether pluralism was a colonial imposition or a constitutional necessity for minority protection.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the zero-sum displacement: the UCC's enactment would extinguish minority communities' law-making authority in the family domain, transferring that authority entirely to the central legislature. The beneficiaries (secular_modernist_coalition, state_legislature) gain unified legislative control and the symbolic capital of 'completing the Constitution.' The victims (minority_religious_communities, personal_law_practitioners, communal_institutions) lose jurisdictional autonomy, interpretive tradition, and the communal cohesion that personal law sustains. Suppression (0.72) is high because the constraint's persistence depends on actively overriding community resistance — through judicial doctrine (e.g., Shah Bano, Shayara Bano), legislative campaigns, and constitutional rhetoric that frames pluralism as 'anomaly.' Theater ratio (0.38) is moderate: the coordination function (gender equality, legal certainty) is real but increasingly instrumentalized; the rhetorical frame of 'completing the Constitution' performs legitimacy while the substantive operation is extraction of communal authority. Accessibility collapse (0.68) is significant: once the UCC frame is accepted, alternatives (pluralism with constitutional floors, community-based reform, consociational guarantees) appear as 'communal appeasement' rather than structural options. Resistance (0.65) is substantial: minority communities, personal law boards, and federalist scholars mount continuous legal, political, and intellectual resistance, but the majoritarian legislative trajectory makes exit from the constraint's logic nearly impossible for the victims.
 *
 * PERSPECTIVAL GAP:
 *   From the secularist seat, the constraint is a rope: genuine coordination solving the problem of fragmented family law, gender inequality under personal codes, and legal uncertainty for inter-community couples. From the minority community seats, the same constraint is a snare: the coordination story is cover for majoritarian capture; the 'gender equality' rationale is selectively deployed against minority codes while majority community practices (Hindu Undivided Family, coparcenary) survive reform. The engine computes this seat divergence from the structural asymmetry — beneficiary declarations for the coalition, victim declarations for communities, and the identity_locked exit that traps communities in the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular_modernist_coalition and state_legislature are structural beneficiaries: they set the agenda (UCC as constitutional mandate), control the legislative timetable, and capture the normative capital of 'modernization.' Their directionality is near-beneficiary (d ~ 0.15) — the constraint subsidizes their authority. Minority_religious_communities are full targets (d ~ 0.95): they bear the extraction (loss of jurisdiction), face active suppression (judicial override, legislative threat), and have no meaningful exit (identity_locked — communal identity is constituted through personal law; leaving it is existential, not optional). Personal_law_practitioners and communal_institutions are similarly targeted (d ~ 0.85–0.90): their professional and institutional existence depends on the pluralist order the constraint aims to eliminate. The derivation chain from beneficiary/victim declarations + exit options produces this directional spread without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate (Article 44's directive principle) was transitional: a constitutional aspiration to be realized when conditions permitted. The mandate has atrophied into a permanent extraction instrument — the 'transitional anomaly' language now serves to delegitimize the pluralist order rather than guide a managed transition. The constraint persists not because the coordination problem remains unsolved (judicial harmonization and community-based reform have produced substantial gender-equality gains within pluralism) but because the extraction function (centralizing legislative authority, capturing the secularist-modernist vote) now drives it. This is mandatrophy resolved: the original function is dead, the extraction function is live, and the constraint is misclassified as coordination when it operates as tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Which structural element of the marriage_authority kernel do the readings genuinely disagree on — the locus of legitimate authority, the function of pluralism, or the remedial pathway?',
    'Map each reading''s claims onto the kernel''s authority_grounding and kernel_codification: does communal_autonomy_reading ground authority in lineage while secularist_reading grounds it in extraction? Does federalist_millet_reading treat the kernel as distributed while secularist_reading treats it as formalized? The disagreement location determines whether readings foreclose, coexist, or influence.',
    'If the disagreement is on authority_grounding (lineage vs. extraction), forecloses relations are likely. If on remedial pathway (UCC legislation vs. judicial harmonization vs. community reform), coexists_with or influences are more accurate. This affects whether the kernel is a site of structural contestation or complementary governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Location of structural disagreement among kernel readings').

omega_variable(
    secularist_foreclosure_of_communal_autonomy,
    'Does the secularist reading''s core premise (legislative supremacy in family law) logically foreclose the communal_autonomy_reading''s core premise (community as normative author) within any single legal framework?',
    'Examine whether a constitutional order can simultaneously hold that (a) marriage authority belongs exclusively to the democratic legislature and (b) religious communities are the authors of family law norms that the state merely enforces. If no framework can reconcile both without internal contradiction, the relation is forecloses; if different parties can hold both as live positions in an ongoing dispute, it is coexists_with.',
    'If forecloses, the kernel contains a genuine logical antinomy — one reading''s triumph requires the other''s structural elimination. If coexists_with, the kernel is a site of persistent pluralistic contestation where neither reading can claim structural finality. This distinction shapes the engine''s foreclosure computation and the classification of the kernel''s drift trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularist_foreclosure_of_communal_autonomy, conceptual, 'Whether secularist_reading forecloses communal_autonomy_reading within a single framework').

omega_variable(
    gender_equality_as_coordination_or_extraction,
    'Is the gender-equality coordination function of the UCC genuine and separable from its extraction function, or is equality rhetoric instrumentally deployed to legitimate communal authority extraction?',
    'Compare gender-equality outcomes under three regimes: (a) UCC jurisdictions (Goa, hypothetically), (b) judicial harmonization within pluralism (Supreme Court''s constitutional floor jurisprudence), (c) community-led reform (e.g., Bohra women''s movement, Muslim women''s organizations). If (b) and (c) achieve comparable equality gains without eliminating communal jurisdiction, the coordination function is separable and the UCC''s extraction is not justified by its coordination. If only (a) delivers substantive equality, the functions are inseparable.',
    'If separable, the constraint''s claimed_type (tangled_rope) is confirmed but the coordination component is smaller than the secularist reading asserts — the extraction is disproportionate. If inseparable, the high extractiveness is the price of the coordination, and the classification might shift toward rope (if beneficiaries include the women gaining equality) or remain tangled_rope (if the beneficiaries are the secularist coalition capturing legislative authority). This omega directly bears on the ε-invariance principle: the constraint''s ε depends on which equality gains are attributed to the UCC vs. alternative pathways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equality_as_coordination_or_extraction, empirical, 'Whether UCC''s gender-equality coordination is genuine or instrumental cover for extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__secularist_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority__secularist_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__secularist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__secularist_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority__secularist_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__secularist_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__secularist_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(marr_be_t1975, marriage_authority__secularist_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__secularist_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__secularist_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(marr_be_t2015, marriage_authority__secularist_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__secularist_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__secularist_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(marr_su_t1975, marriage_authority__secularist_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__secularist_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__secularist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(marr_su_t2015, marriage_authority__secularist_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__secularist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__secularist_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, uniform_civil_code_implementation).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, personal_law_board_authority).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, constitutional_article_44_enforcement).

% DUAL FORMULATION NOTE:
% Part of the marriage_authority kernel family (5 readings). This reading (secularist) claims legislative supremacy and UCC as constitutional completion. Communal_autonomy_reading claims community authority with state enforcement. Federalist_millet_reading claims pluralism as anti-tyranny mechanism. Gender_rights_reading claims judicial reform within pluralism. Judicial_harmonization_reading claims case-by-case constitutional floor. The ε values differ sharply: secularist_reading ε=0.78 (high extraction via UCC), communal_autonomy_reading ε≈0.2 (low extraction, community self-governance), gender_rights_reading ε≈0.4 (moderate extraction via judicial intervention), federalist_millet_reading ε≈0.15 (coordination via pluralism), judicial_harmonization_reading ε≈0.35 (incremental extraction via courts). Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
