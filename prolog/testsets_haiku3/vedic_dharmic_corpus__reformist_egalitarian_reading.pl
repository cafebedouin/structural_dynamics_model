% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Reading of Vedic Dharmic Authority
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   This constraint instantiates the reformist-egalitarian reading of the
 *   Vedic dharmic corpus: the claim that textual meaning must conform to
 *   constitutional equality principles, that caste hierarchy is historical
 *   accretion rather than scriptural essence, and that rational critique
 *   supersedes traditional hereditary authority. This is ONE reading of a
 *   contested kernel shared with the hereditary-monopoly reading (which
 *   asserts caste hierarchy is divinely ordained and textually prescribed)
 *   and the bhakti-devotional reading (which emphasizes direct devotional
 *   access independent of caste). The reformist reading inverts the
 *   traditional beneficiary structure: it benefits Dalit movements,
 *   egalitarian reformers, and the constitutional state apparatus, while
 *   extracting legitimacy from orthodox Brahminical institutions that relied
 *   on the hereditary-monopoly reading for their exclusive authority claim.
 *   The reading is enforced through state judicial pronouncements,
 *   educational curricula, academic gatekeeping, and the constitutional
 *   prohibition on caste discrimination. Extractiveness is moderate (0.45)
 *   because the reading genuinely solves a coordination problem (reconciling
 *   tradition with constitutionalism) while also systematically advantaging
 *   some parties and disadvantaging others. Theater rises to 0.41 at interval
 *   end because as the reading becomes institutionalized, performative
 *   gestures of tradition-fidelity increasingly substitute for substantive
 *   engagement with hereditary-monopoly defenses.
 *
 * KEY AGENTS:
 *   - Dalit movements: Primary beneficiary; mobilize the reformist reading as a legitimacy claim against caste hierarchy
 *   - Constitutional state: Beneficiary and agenda-setter; enforces the reading via courts, legislatures, education systems
 *   - Orthodox Brahminical institutions: Primary payer; lose exclusive authority claim when hereditary lineage is delegitimized
 *   - Egalitarian reformers: Beneficiary; gain professional standing by advancing the reading
 *   - Traditional scholars and pandits: Payer; expertise marginalized when non-specialists can judge readings against constitutional criteria
 *   - Intellectual gatekeepers: Agenda-setter; shape which readings circulate as legitimate in universities and public discourse
 *   - Upper-caste beneficiaries of hierarchy: Excluded; threatened by loss of scriptural legitimacy for privilege
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.52).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic Dharmic Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/interpretive").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61').
narrative_ontology:cs_kernel_codification('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', fixed_text).
narrative_ontology:cs_authority_grounding('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', extraction).
narrative_ontology:cs_interpretation_layer_present('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61').
narrative_ontology:cs_reading_relation('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', foundational, caste_hierarchy_historical_not_essential).
narrative_ontology:cs_axiom_status(caste_hierarchy_historical_not_essential, holdable).
narrative_ontology:cs_axiom_grounding('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', caste_hierarchy_historical_not_essential, empirically_contingent).
narrative_ontology:cs_axiom('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', foundational, constitutional_equality_interpretive_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', constitutional_equality_interpretive_supremacy, deontological).
narrative_ontology:cs_axiom('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', secondary, rational_critique_authority_over_lineage).
narrative_ontology:cs_axiom_status(rational_critique_authority_over_lineage, holdable).
narrative_ontology:cs_axiom_grounding('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', rational_critique_authority_over_lineage, instrumental).
narrative_ontology:cs_reference_frame('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', constitutional_equality_supremacy).
narrative_ontology:cs_drift_state('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', contemporary_institutional_dominance, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('7cd4e8aa-0bfe-46b7-90d5-fafee38b5b61', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, egalitarian_reformers).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_scholars_and_pandits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social movements and political organizations representing communities historically excluded by caste hierarchy. They mobilize the reformist reading as a tool to contest traditional authority and claim legitimate standing as interpreters of Hindu tradition and holders of Hindu identity. They benefit from the reading's assertion that their exclusion is not scripturally mandated, that they can be authentic Hindus without accepting hierarchical positioning, and that constitutional equality has hermeneutical priority. They invest in scholarship, political advocacy, and public education to advance this reading. They work with the state but maintain some organizational autonomy; they can exit state alliances without losing their existence as movements.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, mobile, national).

% Hindu intellectuals, legal scholars, textual scholars, and social activists who hold that Vedic texts can and should be read as compatible with constitutional equality. They include reformed pandits, university professors, legal scholars, and public intellectuals. They gain professional standing, academic voice, institutional positions, and moral authority by advancing this reading. Their careers depend partly on the reading's acceptance in academic, legal, and policy institutions; they cannot easily exit without significant professional cost. They face sustained counter-argument from orthodox institutions but operate within protected institutional spaces (universities, civil-rights NGOs, state advisory bodies) that amplify their voice.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, egalitarian_reformers, beneficiary,
    moderate, biographical, constrained, national).

% The post-independence Indian state and its institutions (courts, legislatures, administrative agencies, public education system). The state benefits from the reformist reading as a solution to a foundational legitimacy problem: how to enforce constitutional equality while claiming fidelity to Hindu civilization and tradition. The reading allows the state to appear as protector of Hindu tradition (not its destroyer) while enforcing anti-caste law. The state actively promotes and enforces this reading through judicial pronouncements citing it in caste-discrimination cases, constitutional-law curricula that present it as the correct reading, and administrative policies that exclude hereditary-monopoly readings from state platforms and schools. The state collects legitimacy from this reading's institutional dominance; it has power but operates within constitutional constraints.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state, agenda_setter).

% Traditional hereditary priesthoods, ritual authorities, temples, monastic institutions (mathas), and religious organizations that claim exclusive interpretive authority grounded in birth into Brahmin lineage and transmission of sacred knowledge. They are payers because the reformist reading directly contests their monopoly on legitimate interpretation and authority. When the reading gains institutional ground, their traditional sources of authority—control over ritual interpretation, exclusive access to sacred knowledge, hereditary transmission as the sole path to authority—are delegitimized. They face pressure from state judicial decisions that cite the reformist reading, from public education that teaches the reformist reading as authoritative, and from intellectual gatekeeping that marginalizes their scholarship. They cannot exit without ceasing to exist as traditional authorities.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions, payer,
    powerful, generational, constrained, national).

% Textual scholars and ritual specialists trained in traditional hermeneutics and claiming to read the Vedas as prescribing varna (caste) hierarchy as divinely ordained. They experience the reformist reading as systematically marginalizing their expertise: it reframes their scholarship as ideologically captured or parochial rather than transparent textual exegesis. The reading asserts that non-specialists can and should judge their interpretations against constitutional principles, removing the epistemic gatekeeping that made their expertise legible and valuable. Their professional identity, social standing within orthodox communities, and sense of intellectual authority all depend on the hereditary-monopoly reading; accepting the reformist reading would require dissolving the framework that makes their knowledge meaningful. They are identity-locked: they cannot exit without losing who they are as traditional scholars.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_scholars_and_pandits, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_scholars_and_pandits, excluded).

% Castes positioned at the top of the traditional varna hierarchy (Brahmins, Kshatriyas) who have historically benefited from caste-based ritual status, control over knowledge, marriage rules, and preferential access to resources and positions. The reformist reading threatens to erode the scriptural legitimacy of their privilege by reframing caste hierarchy as historical accident rather than cosmic necessity. Some have adapted by adopting egalitarian rhetoric and constitutional equality language in public while maintaining discriminatory practice in private; others actively oppose the reading and defend the hereditary-monopoly interpretation. They are excluded from the beneficiary list because the reformist reading's entire logic requires inverting the traditional beneficiary structure: what benefited them under the hereditary reading disadvantages them under the reformist reading. They have power and mobility but face delegitimization of the reading that formerly justified their privilege.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, upper_caste_beneficiaries_of_hierarchy, excluded,
    powerful, generational, mobile, national).

% Constitutional courts and legislative bodies that enforce anti-caste law and interpret the Vedas through the lens of constitutional equality. They actively promote the reformist reading through judicial decisions, legislation, and constitutional pronouncements. They cite the reading in judgments on caste discrimination; they mandate reformist content in state-controlled religious education; they explicitly delegitimize hereditary-monopoly readings in official discourse. Their enforcement machinery includes constitutional protections for Dalit mobility, workplace and marriage protections against caste discrimination, and exclusion of caste-based ritual hierarchy from state recognition and subsidy. They set the institutional agenda by controlling what gets heard in courts, legislatures, and state platforms.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, state_courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Academics, university hiring committees, academic publishing houses, peer-review systems, museum curators, cultural authorities, and intellectual institutions that shape which readings of Hindu texts circulate as legitimate and authoritative. Universities privilege reformist scholarship in hiring and promotion; academic presses publish egalitarian reinterpretations and marginalize hereditary readings; peer review processes favor reformist methodology and delegitimize traditional hermeneutics. Museum exhibitions present Vedic texts as egalitarian-compatible rather than hierarchy-prescribing. These gatekeepers enforce the reading's dominance in institutional intellectual discourse, making the hereditary-monopoly reading appear parochial, ideologically captured, or discredited in public-facing academic spaces. They exercise moderate power but their institutional positions are dependent on maintaining alignment with state and constitutional frameworks.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, intellectual_consensus_gatekeepers, agenda_setter,
    moderate, biographical, constrained, national).

% Communities organized around bhakti (devotional) theology and practice—movements claiming direct access to the divine independent of caste, ritual hierarchy, or hereditary authority. They occupy an intermediate position: they affirm that caste is not absolutely binding on spiritual status and access, which brings them into partial alignment with the reformist reading. However, they do not necessarily adopt the reformist reading's constitutional framing, rational-critique priority, or state-centered enforcement mechanism. They see the reformist reading as compatible with their own theology but neither wholly endorse nor oppose it. Their presence complicates the binary opposition between hereditary-monopoly and reformist-egalitarian readings; they represent a third live alternative. They are observers because they are affected by this constraint but do not fit cleanly into its beneficiary-victim structure; they benefit from delegitimization of caste hierarchy but may resist the reformist reading's rationalism and state partnership.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_practitioners, observer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconcile post-independence India's constitutional commitment to equality with the authority and legitimacy of Hindu theological tradition. Provide a framework allowing the state to enforce anti-caste law while claiming fidelity to Hindu civilization and the Vedas. Enable Dalit and non-Brahmin communities to claim standing as authentic interpreters of their own religious tradition rather than as rejectors of tradition. Coordinate between secularism and tradition-respecting reform.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from hereditary Brahminical priesthoods and traditional scholars to a broader constituency: constitutional scholars, state institutions, Dalit intellectuals, egalitarian-reformer pandits, and academic experts who use rational critique and constitutional principles as interpretive tools. Transfers legitimacy from hereditary lineage and traditional transmission to textual scholarship, constitutional alignment, and rational argumentation. Moves the beneficiary structure from castes at the top of the hierarchy to Dalit movements and egalitarian-reformer institutions.
% ABSENT_VOICES: Orthodox hereditary priesthoods and traditional-authority defenders are named as structural payers (systematically disadvantaged in state and academic institutional hierarchies), but they are significantly marginalized in the public discourse where this reading is promoted. Their sophisticated textual defenses are often excluded from state platforms, public education, and academic publishing. Rural and village-level practitioners of caste-based ritual, and community religious specialists who lack elite institutional connections, often have no voice in the state and academic institutions where this reading dominates. Upper-caste communities who benefited from the hereditary reading are excluded from beneficiary standing but retain power to resist.
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished and the hereditary-monopoly reading regained state and institutional endorsement, the post-independence constitutional framework for anti-caste law would lose its primary claimed grounding in Hindu theology; enforcement would appear as purely secular imposition rather than fidelity to tradition. Dalit movements would lose a crucial legitimacy claim: they could no longer assert that they are authentic Hindus reclaiming the egalitarian essence of their own tradition; instead they would appear to be rejecting Hinduism altogether. The state would face pressure to either abandon anti-caste law enforcement (losing its constitutional mandate) or justify it purely on secular grounds (weakening the state's claim to cultural continuity and Hindu civilization fidelity). The institutional and intellectual landscape—universities, courts, state education systems, publishing—would reorganize around different hermeneutical authorities: traditional scholars would regain gatekeeping power, egalitarian-reformer scholarship would be marginalized as ideologically driven, Brahminical institutions would regain legitimacy authority. The distribution of intellectual authority and cultural legitimacy would be inverted.
% FOUNDING_PROBLEM: After independence, India faced a fundamental legitimacy crisis at the intersection of law and culture: the Constitution mandated caste-blind equality and prohibited caste discrimination, but the dominant Hindu theological tradition (as interpreted by hereditary authorities) appeared to prescribe caste hierarchy as divinely ordained. This created two urgent problems: (1) How could the new state claim fidelity to Hindu civilization and tradition while enforcing laws that appeared to contradict that tradition? (2) How could non-Brahmin and Dalit communities claim standing as legitimate interpreters of and participants in their own religious tradition, rather than appearing as external rejectors of Hinduism? The reformist reading solved both problems simultaneously by asserting that scriptural essence supports equality, that caste hierarchy is historical accretion layered onto egalitarian core principles, and that rational critique and constitutional principles have authority to govern interpretation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality at t=0 is attested by independent observers: constitutional historians and scholars of Indian legal history document the state's acute need for a legitimacy framework (sources outside the benefiting parties confirm this was a genuine structural problem, not invented for rhetorical advantage). Historians of Hindu modernism and Indian intellectual history attest that reformist reinterpretation was a genuine intellectual response to colonial experience and post-independence governance challenges, not a cynical tool created ex nihilo. The problem's continued urgency through the interval is actively contested: Dalit movements and egalitarian reformers attest it remains live (traditional hereditary readings still circulate in communities, caste discrimination persists in practice despite law, and the state must continuously enforce and re-legitimize anti-caste law against counter-pressure). Orthodox institutions attest the problem was artificially created by secular state intrusion and Western influence, not native to the logic of Hindu philosophy itself; they experience the reading as an attack on tradition, not a solution to a problem. No corroboration from hereditary-monopoly defenders that they recognize the founding problem as genuinely binding on them (they reject the problem's framing).
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.45 reflects a genuine coordination achievement (reconciling tradition with equality) coupled with systematic inversion of authority: the reading transfers legitimacy from hereditary priesthoods to state institutions and Dalit scholars. This is not pure extraction (which would show higher ε and lower accessibility_collapse) because the reading genuinely opens interpretive space and solves the post-independence legitimacy crisis. But it is not pure coordination (which would show lower ε and higher resistance from beneficiaries) because the transfer of authority is enforced against structured resistance from orthodox institutions. Suppression rises from 0.38 to 0.52 over the interval as the state's enforcement machinery hardens: early reformist arguments operated as intellectual persuasion; later enforcement includes constitutional exclusions of hereditary-monopoly readings from state schools, judicial delegitimization of traditional authorities in caste-discrimination cases, and institutional gatekeeping that marginalizes orthodox scholarship. Theater rises to 0.41 and plateaus because as the reading becomes institutionalized orthodoxy, state performances of 'respecting tradition' accumulate while substantive engagement with orthodox counter-arguments declines. Accessibility_collapse is moderate (0.48) because the reformist reading does not eliminate the hereditary-monopoly reading—it coexists with it in different institutional spaces and community contexts—but it does collapse alternatives in official and academic discourse. Resistance is high (0.67) because the reading faces sustained challenge from orthodox institutions with significant power and social embeddedness; the fact that the reading persists despite high resistance indicates robust institutional backing (state apparatus, academic institutions) rather than voluntary adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the constitutional state and Dalit-movement seats, the reading is primarily a coordination achievement: it legitimizes equality-enforcement within Hindu tradition. From the orthodox Brahminical seats, the reading is experienced as extractive delegitimization: their authority is not merely contested but systematically disadvantaged in state and academic institutional hierarchies. A powerful reformist seat (a tenured scholar with state endorsement) experiences the reading as liberation and truth-telling; an equally-educated traditional scholar experiences the same reading as ideologically-driven suppression of legitimate hermeneutical alternatives. The engine should compute these seats as experiencing significantly different directionalities: the reformist beneficiary approaches d=0.0 (full beneficiary of the reading's institutional dominance), while the orthodox payer approaches d=1.0 (full target of the reading's systematic delegitimization). Neither seat is incorrect about what is happening; they describe the same constraint from asymmetrically positioned feet.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit movements are beneficiaries with organized power and mobile exit (they mobilize the reading but are not dependent on any single institution for existence). Their directionality is low (toward beneficiary end): they gain authority and legitimacy from the reading, have institutional backing from state and civil-rights organizations, and face no suppression for advocating this reading. Egalitarian reformers are beneficiaries with moderate power and constrained exit (their careers depend partly on institutional positions that endorse the reading). Their directionality is slightly higher than Dalit movements but still beneficiary-skewed (d ~0.25-0.35): they benefit from the reading's institutional dominance but cannot freely leave because their expertise is premised on its acceptance. Orthodox Brahminical institutions are victims with powerful institutional resources but constrained exit (they cannot abandon their hereditary claims without ceasing to exist as traditional authorities). Their directionality is high (toward target end, d ~0.75-0.85): the reading systematically delegitimizes their exclusive authority, they face institutional pressure from state and academic gatekeeping, and they resist the reading actively. Traditional scholars are victims with moderate power and strongly identity-locked exit (their professional identity and social standing within orthodox communities depend entirely on the hereditary-monopoly reading; accepting the reformist reading would mean professional dissolution). Their directionality is very high (d ~0.85-0.95): the reading is directly destructive of their epistemic authority, they experience high suppression (intellectual marginalization), and they have almost no exit that preserves their identity. Constitutional state is simultaneously beneficiary (it solves a legitimacy crisis) and agenda-setter (it enforces the reading actively). Its directionality is beneficiary-skewed because it collects legitimacy from the reading's institutional dominance, but its role as enforcer introduces slight target characteristics (it must expend resources to maintain suppression of hereditary-monopoly readings in official spaces). Directionality for the state is approximately d~0.30-0.40.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading solves a live founding problem at t=0 (post-independence legitimacy crisis: how to reconcile tradition with equality) and the problem remains contested but urgent through the interval. However, the reading's mechanism has shifted over 75 years: early (t=0-30) it operated primarily as intellectual persuasion and reformist theology—a genuine coordination function. By t=75 it has shifted toward institutional enforcement and gatekeeping—the state and academic institutions actively suppress the hereditary-monopoly reading rather than competing with it on merits. This is not mandatrophy (the founding problem has not been solved and declared obsolete) but it is mission-drift: the reading's justification remains the coordination problem (tradition + equality), but its actual operation increasingly consists of suppressing an alternative reading. The theater_ratio climb from 0.22 to 0.41 documents this drift: the reading's claim to be 'just good exegesis' weakens as its institutional dominance becomes obvious and alternatives are formally excluded from state platforms. Mandatrophy is not yet achieved because (a) the hereditary-monopoly reading has not disappeared, so the coordination problem remains live, and (b) Dalit movements and egalitarian reformers still experience the reading as contested and requiring defense, not as settled truth. However, if the hereditary-monopoly reading were to vanish from state-backed institutions and oral Hindu practice, and if upper-caste beneficiaries of hierarchy ceased to defend caste on scriptural grounds, the founding problem would be solved and the reading could face mandatrophy pressure (what would justify its continued institutional enforcement if the traditional authority structure it opposes no longer claims to exist?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_essence_vs_accretion,
    'Is caste hierarchy an essential feature of Vedic theology or a historical accretion layered onto egalitarian core principles?',
    'Historical-textual analysis: comparison of early Vedic passages with later Brahminical elaborations; examination of variant readings and lost textual traditions; cross-cultural comparative study of how similar texts are interpreted across communities with different power structures.',
    'If hierarchy is essential, the hereditary-monopoly reading is closer to textual fidelity, and the reformist reading is rewriting rather than recovering. If hierarchy is accretion, the reformist reading claims genuine exegetical authority and the hereditary reading is ideologically captured. This is the foundational ambiguity distinguishing the two readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_essence_vs_accretion, conceptual, 'Whether caste hierarchy is intrinsic to Vedic philosophy or historically added.').

omega_variable(
    constitutional_supremacy_hermeneutic_authority,
    'Does constitutional equality principle have authority to govern scriptural interpretation, or do religious texts maintain interpretive autonomy from secular law?',
    'Jurisprudential principle: this is a preference-class omega grounded in how one weighs religious liberty vs. equal protection. Different democracies have different answers (Canada prioritizes religious accommodation; France prioritizes secular law). Resolution requires political and constitutional choice, not empirical data.',
    'If constitutional principles govern interpretation, the reformist reading''s rational-critique framework is legitimate. If texts maintain interpretive autonomy, the hereditary reading''s claim to textual self-determination is strengthened. This distinguishes how the two readings allocate interpretive authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_supremacy_hermeneutic_authority, preference, 'Whether constitutional law can override religious interpretive autonomy.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is orthodox resistance to the reformist reading primarily structural (gatekeeping and institutional exclusion) or internalized (orthodox believers have genuinely accepted the reformist framework)?',
    'Post-suppression trajectory: in institutional spaces where the reformist reading is not enforced (rural communities, unregistered temples, private study circles), do hereditary-monopoly readings persist or have they been internalized-away? If they persist, suppression is mainly structural. If they have declined, suppression has become partially internalized.',
    'If suppression is mainly structural, relaxing institutional gatekeeping would quickly revive hereditary readings. If internalized, the constraint''s persistence does not depend on active enforcement. This affects whether the reading is more properly classified as tangled_rope (requiring active enforcement) or snare (suppression-dependent) vs. rope (voluntarily adopted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether orthodox resistance is kept down by external gatekeeping or by internalized acceptance of the reformist framework.').

omega_variable(
    dalit_authentic_voice_vs_state_ventriloquism,
    'To what extent do Dalit movements autonomously hold and advance the reformist reading, vs. the state deploying ''Dalit voice'' rhetorically while controlling institutional implementation?',
    'Political-ethnographic study: examine cases where Dalit movements have resisted or redirected state enforcement; document instances of divergence between Dalit organizing and state constitutional doctrine; assess whether state institutions listen when Dalit actors advance non-reformist or extra-reformist readings.',
    'If Dalit agency is substantial, Dalit movements are genuine beneficiaries. If the state is primarily instrumentalizing Dalit voice, Dalit movements may be partly coopted—benefiting from legitimacy gain but losing control over the reading''s direction. This affects whether Dalit movements should be classified as beneficiaries or as partly-payer seats used by the state agenda-setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dalit_authentic_voice_vs_state_ventriloquism, empirical, 'Whether Dalit movements are autonomous beneficiaries or partly-instrumentalized by state enforcement.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this constraint best understood as a ''reading'' of a stable kernel (the Vedic texts remain unchanged; interpretations vary) or as a hermeneutical remaking that is itself a different constraint?',
    'Philosophy of language choice: if readings are interpretations of a fixed text, the kernel is the text and readings are different constraints (per the ε-invariance principle). If readings are generative reinterpretations that remake the text''s meaning, then the distinction between kernel and reading collapses and there is only one constraint that changes over time. This is not empirically resolvable; it is a framing choice.',
    'If the kernel/reading distinction is valid, the hereditary-monopoly and reformist readings are genuinely different constraints (different ε, different beneficiary structures, linked by network.affects_constraints). If readings are remakings, there is one evolving constraint (Vedic authority in Indian society) undergoing transformations. The committer frame assumes the first framing; the second framing would require collapsing the three readings into one story with time-varying ε and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether this is a reading of a stable kernel or a hermeneutical remaking that is itself the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(vedi_tr_t10, observed).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(vedi_tr_t20, observed).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(vedi_tr_t30, observed).
narrative_ontology:measurement(vedi_tr_t45, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement_basis(vedi_tr_t45, observed).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t60, observed).
narrative_ontology:measurement(vedi_tr_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(vedi_be_t10, observed).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(vedi_be_t20, observed).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(vedi_be_t30, observed).
narrative_ontology:measurement(vedi_be_t45, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 45, 0.46).
narrative_ontology:measurement_basis(vedi_be_t45, observed).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement_basis(vedi_be_t60, observed).
narrative_ontology:measurement(vedi_be_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement_basis(vedi_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(vedi_su_t10, observed).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(vedi_su_t20, observed).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(vedi_su_t30, observed).
narrative_ontology:measurement(vedi_su_t45, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement_basis(vedi_su_t45, observed).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(vedi_su_t60, observed).
narrative_ontology:measurement(vedi_su_t75, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement_basis(vedi_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel decomposes into three structurally distinct constraints: (1) reformist-egalitarian reading (THIS story) — claims caste is historical accretion, ε=0.45, benefits Dalit movements and constitutional state, extractive via delegitimization of orthodox authority; (2) hereditary-monopoly reading — claims caste is divinely ordained, ε>0.7 (high extraction), benefits Brahminical institutions, enforced via institutional control and hereditary gatekeeping; (3) bhakti-devotional reading — claims devotion bypasses caste, moderate ε, intermediate beneficiary structure. Each reading is a distinct constraint with its own ε, beneficiary/victim structure, and classification. They are not the same constraint viewed from different angles; the ε values differ substantially (ε-invariance principle: if changing the reading changes ε, you have different constraints). All three affect each other: the reformist reading forecloses the hereditary reading in constitutional spaces, influences the bhakti reading by providing institutional backing, and is contested by the hereditary reading in academic and community spaces. The three readings together instantiate a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
